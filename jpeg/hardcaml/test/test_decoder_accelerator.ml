open Core
open Hardcaml
open Hardcaml_waveterm
open Hardcaml_jpeg
open Hardcaml_jpeg_model
module Accl = Decoder_accelerator
module Sim = Cyclesim.With_interface (Accl.I) (Accl.O)

let display_rules =
  let module I = Display_rules.With_interface (Accl.I) in
  let module O = Display_rules.With_interface (Accl.O) in
  Display_rule.[ I.default (); O.default (); [ default ] ] |> List.concat
;;

let get_entropy_coded_segment bits =
  let sos = Util.find_next_marker_exn ~start_pos:0 ~marker_code:Marker_code.sos bits in
  let sos_length = (Char.to_int bits.[sos + 2] lsl 8) lor Char.to_int bits.[sos + 3] in
  let ecs = sos + 2 + sos_length in
  let eoi = Util.find_next_marker_exn ~start_pos:ecs ~marker_code:Marker_code.eoi bits in
  String.sub ~pos:ecs ~len:(eoi - ecs) bits
;;

let get_headers_and_model_and_bits jpeg =
  let bits = Util.load_jpeg_file jpeg in
  let headers = Decoder.Header.decode bits in
  let decoder = Decoder.init headers bits in
  let model = Decoder.For_testing.Sequenced.decode decoder in
  let bits = Bitstream_reader.From_string.get_buffer bits in
  headers, model, get_entropy_coded_segment bits
;;

let ( <--. ) a b = a := Bits.of_int ~width:(Bits.width !a) b

let max_reconstructed_diff pixels recon =
  Array.fold2_exn pixels recon ~init:0 ~f:(fun acc a b ->
      let diff = Int.abs (a - b) in
      if diff < acc then acc else diff)
;;

let test ?(waves = true) ?(error_tolerance = 2) ?num_blocks_to_decode jpeg =
  let headers, model, data = get_headers_and_model_and_bits jpeg in
  let width = (Decoder.Header.frame headers |> Option.value_exn).width in
  let height = (Decoder.Header.frame headers |> Option.value_exn).height in
  print_s [%message (width : int) (height : int)];
  let frame = Frame.create ~chroma_subsampling:C420 ~width ~height in
  let sim =
    Sim.create
      ~config:Cyclesim.Config.trace_all
      (Accl.create
         (Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true ()))
  in
  let waves, sim =
    if waves
    then (
      let waves, sim = Waveform.create sim in
      Some waves, sim)
    else None, sim
  in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let outputs_before = Cyclesim.outputs ~clock_edge:Before sim in
  inputs.clocking.clear := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clocking.clear := Bits.gnd;
  let huffman_tables = Decoder.Header.huffman_tables headers in
  Util.load_huffman_tables ~cycle:(fun () -> Cyclesim.cycle sim) inputs.dht huffman_tables;
  let quant_tables = Decoder.Header.quant_tables headers in
  Util.load_quant_tables ~cycle:(fun () -> Cyclesim.cycle sim) inputs.dqt quant_tables;
  inputs.jpeg_valid := Bits.vdd;
  let pos = ref 0 in
  let get_data () =
    let d0 =
      try Char.to_int data.[(!pos * 2) + 0] with
      | _ -> 0
    in
    let d1 =
      try Char.to_int data.[(!pos * 2) + 1] with
      | _ -> 0
    in
    Bits.of_int ~width:16 ((d0 lsl 8) lor d1)
  in
  inputs.jpeg := get_data ();
  Int.incr pos;
  let cycle () =
    Cyclesim.cycle sim;
    if Bits.to_bool !(outputs_before.jpeg_ready)
    then (
      inputs.jpeg := get_data ();
      Int.incr pos)
  in
  let upload_pixels () =
    let pixels = ref [] in
    let add_pixel () = pixels := Bits.to_int !(outputs.pixel) :: !pixels in
    inputs.pixel_read_enable := Bits.vdd;
    for i = 0 to 63 do
      inputs.pixel_read_address <--. i;
      cycle ();
      add_pixel ()
    done;
    inputs.pixel_read_enable := Bits.gnd;
    Array.of_list (List.rev !pixels)
  in
  let dc_pred = Array.init 3 ~f:(Fn.const 0) in
  let get_dc_pred block =
    let subblock = block % 6 in
    if subblock < 4
    then dc_pred.(0)
    else if subblock = 4
    then dc_pred.(1)
    else dc_pred.(2)
  in
  let set_dc_pred block dc_pred_out =
    let subblock = block % 6 in
    if subblock < 4
    then dc_pred.(0) <- dc_pred_out
    else if subblock = 4
    then dc_pred.(1) <- dc_pred_out
    else dc_pred.(2) <- dc_pred_out
  in
  let run_one_block block =
    let subblock = block % 6 in
    inputs.start_codeblock_decoder := Bits.vdd;
    inputs.start_idct := Bits.vdd;
    inputs.ac_table_select := Bits.of_int ~width:2 (if subblock >= 4 then 1 else 0);
    inputs.dc_table_select := Bits.of_int ~width:2 (if subblock >= 4 then 1 else 0);
    inputs.qnt_table_select := Bits.of_int ~width:2 (if subblock >= 4 then 1 else 0);
    inputs.dc_pred_in <--. get_dc_pred block;
    cycle ();
    inputs.start_codeblock_decoder := Bits.gnd;
    inputs.start_idct := Bits.gnd;
    cycle ();
    let pixels = upload_pixels () in
    while not (Bits.to_bool !(outputs.done_)) do
      cycle ()
    done;
    let dc_pred_out = Bits.to_int !(outputs.dc_pred_out) in
    set_dc_pred block dc_pred_out;
    pixels
  in
  (* initialize the decoder pipeline *)
  ignore (run_one_block 0 : int array);
  ignore (run_one_block 1 : int array);
  let rec decode_and_compare_with_model model block_number =
    match num_blocks_to_decode with
    | Some num_blocks_to_decode when block_number >= num_blocks_to_decode ->
      (* finish early *) ()
    | Some _ | None ->
      (match Sequence.hd model with
      | None -> () (* finished decoding *)
      | Some comp ->
        (* Run simulator *)
        let pixels = run_one_block (block_number + 2) in
        (* print_s [%message (pixels : Int.Hex.t array)]; *)
        Util.reconstruct ~block_number frame pixels;
        let max_reconstructed_diff =
          max_reconstructed_diff pixels (Decoder.Component.recon comp)
        in
        if max_reconstructed_diff >= error_tolerance
        then
          print_s
            [%message
              (block_number : int)
                (max_reconstructed_diff : int)
                (pixels : Decoder.Component.Summary.pixel_block)
                (comp : Decoder.Component.Summary.t)];
        decode_and_compare_with_model (Sequence.tl_eagerly_exn model) (block_number + 1))
  in
  try
    decode_and_compare_with_model model 0;
    for _ = 0 to 10 do
      Cyclesim.cycle sim
    done;
    frame, waves
  with
  | e ->
    print_s [%message "Failed" (e : exn)];
    frame, waves
;;

let%expect_test "" =
  let _, waves =
    test ~waves:true ~num_blocks_to_decode:6 ~error_tolerance:0 "Mouse480.jpg"
  in
  Option.iter
    waves
    ~f:
      (Waveform.print
         ~display_rules
         ~display_width:120
         ~display_height:75
         ~wave_width:(-30));
  [%expect
    {|
    ((width 480) (height 320))
    ((macroblock 0) (subblock 0) (block_number 0) (x_pos 0) (y_pos 0))
    ((block_number 0) (max_reconstructed_diff 1)
     (pixels
      ((e0 de db d6 d1 cd ca c8) (e4 e2 df db d7 d3 cf ce)
       (e9 e8 e6 e2 df db d9 d7) (ee ed eb e9 e6 e4 e2 e1)
       (ef ef ee ec eb ea e9 e8) (ed ed ed ec ec ec ec eb)
       (e9 e9 ea ea eb eb eb ec) (e6 e7 e7 e8 e9 ea eb eb)))
     (comp
      ((x 0) (y 0) (dc_pred 20) (component.identifier 1)
       (coefs
        ((014 001 ffe fff 001 000 000 000) (000 000 000 000 000 000 000 000)
         (000 000 000 000 000 000 000 000) (000 000 000 000 000 000 000 000)
         (000 000 000 000 000 000 000 000) (000 000 000 000 000 000 000 000)
         (000 000 000 000 000 000 000 000) (000 000 000 000 000 000 000 000)))
       (dequant
        ((320 01c 000 000 000 000 000 000) (fc4 01e 000 000 000 000 000 000)
         (fdd 000 000 000 000 000 000 000) (000 000 000 000 000 000 000 000)
         (000 000 000 000 000 000 000 000) (000 000 000 000 000 000 000 000)
         (000 000 000 000 000 000 000 000) (000 000 000 000 000 000 000 000)))
       (idct
        ((60 5e 5b 56 51 4d 4a 48) (64 62 5f 5b 57 53 50 4e)
         (69 68 66 62 5f 5b 59 58) (6e 6d 6b 69 66 64 62 61)
         (6f 6f 6e 6c 6b 6a 69 68) (6d 6d 6d 6c 6c 6c 6c 6b)
         (69 69 6a 6a 6b 6b 6c 6c) (66 67 67 68 69 6a 6b 6b)))
       (recon
        ((e0 de db d6 d1 cd ca c8) (e4 e2 df db d7 d3 d0 ce)
         (e9 e8 e6 e2 df db d9 d8) (ee ed eb e9 e6 e4 e2 e1)
         (ef ef ee ec eb ea e9 e8) (ed ed ed ec ec ec ec eb)
         (e9 e9 ea ea eb eb ec ec) (e6 e7 e7 e8 e9 ea eb eb))))))
    ((macroblock 0) (subblock 1) (block_number 1) (x_pos 0) (y_pos 0))
    ((block_number 1) (max_reconstructed_diff 1)
     (pixels
      ((bf bf bf bf bf bf bf bf) (cf cf cf cf cf cf cf cf)
       (e3 e3 e3 e3 e3 e3 e3 e3) (ef ef ef ef ef ef ef ef)
       (f0 f0 f0 f0 f0 f0 f0 f0) (ee ee ee ee ee ee ee ee)
       (f0 f0 f0 f0 f0 f0 f0 f0) (f2 f2 f2 f2 f2 f2 f2 f2)))
     (comp
      ((x 8) (y 0) (dc_pred 20) (component.identifier 1)
       (coefs
        ((000 000 ffc ffe 000 000 000 000) (000 fff 000 000 000 000 000 000)
         (000 000 000 000 000 000 000 000) (000 000 000 000 000 000 000 000)
         (000 000 000 000 000 000 000 000) (000 000 000 000 000 000 000 000)
         (000 000 000 000 000 000 000 000) (000 000 000 000 000 000 000 000)))
       (dequant
        ((320 000 000 000 000 000 000 000) (f88 000 000 000 000 000 000 000)
         (fba 000 000 000 000 000 000 000) (fdd 000 000 000 000 000 000 000)
         (000 000 000 000 000 000 000 000) (000 000 000 000 000 000 000 000)
         (000 000 000 000 000 000 000 000) (000 000 000 000 000 000 000 000)))
       (idct
        ((3f 3f 3f 3f 3f 3f 3f 3f) (4f 4f 4f 4f 4f 4f 4f 4f)
         (63 63 63 63 63 63 63 63) (6f 6f 6f 6f 6f 6f 6f 6f)
         (70 70 70 70 70 70 70 70) (6e 6e 6e 6e 6e 6e 6e 6e)
         (70 70 70 70 70 70 70 70) (73 73 73 73 73 73 73 73)))
       (recon
        ((bf bf bf bf bf bf bf bf) (cf cf cf cf cf cf cf cf)
         (e3 e3 e3 e3 e3 e3 e3 e3) (ef ef ef ef ef ef ef ef)
         (f0 f0 f0 f0 f0 f0 f0 f0) (ee ee ee ee ee ee ee ee)
         (f0 f0 f0 f0 f0 f0 f0 f0) (f3 f3 f3 f3 f3 f3 f3 f3))))))
    ((macroblock 0) (subblock 2) (block_number 2) (x_pos 0) (y_pos 0))
    ((block_number 2) (max_reconstructed_diff 1)
     (pixels
      ((c6 cb d4 dd e4 e8 e9 ea) (b3 b7 bd c6 d0 da e2 e6)
       (a5 a5 a6 ac b7 c5 d3 dc) (af aa a4 a2 a8 b5 c3 cd)
       (ca c2 b6 ad ab b0 b9 c0) (de d7 cc c1 ba b8 b9 bb)
       (de dc d7 d1 cb c5 c1 bf) (d6 d7 d8 d8 d5 cf c9 c6)))
     (comp
      ((x 0) (y 8) (dc_pred 14) (component.identifier 1)
       (coefs
        ((ffa fff 000 003 ffd 001 000 000) (000 001 000 001 fff 000 000 000)
         (000 000 000 000 000 000 000 000) (000 000 000 000 000 000 000 000)
         (000 000 000 000 000 000 000 000) (000 000 000 000 000 000 000 000)
         (000 000 000 000 000 000 000 000) (000 000 000 000 000 000 000 000)))
       (dequant
        ((230 fe4 019 000 000 000 000 000) (000 fa6 000 000 000 000 000 000)
         (069 000 fd8 000 000 000 000 000) (023 02b 000 000 000 000 000 000)
         (000 000 000 000 000 000 000 000) (000 000 000 000 000 000 000 000)
         (000 000 000 000 000 000 000 000) (000 000 000 000 000 000 000 000)))
       (idct
        ((46 4b 54 5d 64 68 6a 6a) (33 37 3d 46 50 5a 62 66)
         (25 25 26 2c 37 45 53 5c) (2f 2a 24 22 28 35 43 4d)
         (4a 42 36 2d 2b 30 39 40) (5e 57 4c 41 3a 38 39 3b)
         (5e 5c 57 51 4b 45 41 3f) (56 57 58 58 55 4f 49 46)))
       (recon
        ((c6 cb d4 dd e4 e8 ea ea) (b3 b7 bd c6 d0 da e2 e6)
         (a5 a5 a6 ac b7 c5 d3 dc) (af aa a4 a2 a8 b5 c3 cd)
         (ca c2 b6 ad ab b0 b9 c0) (de d7 cc c1 ba b8 b9 bb)
         (de dc d7 d1 cb c5 c1 bf) (d6 d7 d8 d8 d5 cf c9 c6))))))
    ((macroblock 0) (subblock 3) (block_number 3) (x_pos 0) (y_pos 0))
    ((block_number 3) (max_reconstructed_diff 1)
     (pixels
      ((d7 e0 ec f6 f9 f4 ec e5) (d7 dd e5 ed f1 f2 f0 ee)
       (d2 d3 d5 da e1 e9 f1 f5) (c4 c1 be c0 c8 d7 e6 f0)
       (b4 af a9 a9 b2 c2 d4 e0) (ab a7 a2 a1 a8 b6 c5 cf)
       (ae ab a8 a8 ad b5 be c4) (b3 b2 b1 b2 b5 b9 be c1)))
     (comp
      ((x 8) (y 8) (dc_pred 15) (component.identifier 1)
       (coefs
        ((001 ffd 006 001 000 001 000 fff) (001 fff 000 000 fff 000 000 000)
         (000 000 000 000 000 000 000 000) (000 000 000 000 000 000 000 000)
         (000 000 000 000 000 000 000 000) (000 000 000 000 000 000 000 000)
         (000 000 000 000 000 000 000 000) (000 000 000 000 000 000 000 000)))
       (dequant
        ((258 fac 019 000 000 000 000 000) (0b4 000 fdd 000 000 000 000 000)
         (023 021 fd8 000 000 000 000 000) (fdd 000 000 000 000 000 000 000)
         (000 000 000 000 000 000 000 000) (000 000 000 000 000 000 000 000)
         (000 000 000 000 000 000 000 000) (000 000 000 000 000 000 000 000)))
       (idct
        ((57 60 6c 76 79 74 6c 66) (57 5d 65 6d 71 72 70 6e)
         (52 53 55 5a 61 69 71 75) (44 41 3e 3f 48 57 66 70)
         (34 2f 29 29 32 42 54 60) (2b 27 22 21 28 36 45 4f)
         (2e 2b 28 28 2d 35 3e 44) (33 32 31 32 35 39 3e 41)))
       (recon
        ((d7 e0 ec f6 f9 f4 ec e6) (d7 dd e5 ed f1 f2 f0 ee)
         (d2 d3 d5 da e1 e9 f1 f5) (c4 c1 be bf c8 d7 e6 f0)
         (b4 af a9 a9 b2 c2 d4 e0) (ab a7 a2 a1 a8 b6 c5 cf)
         (ae ab a8 a8 ad b5 be c4) (b3 b2 b1 b2 b5 b9 be c1))))))
    ((macroblock 0) (subblock 4) (block_number 4) (x_pos 0) (y_pos 0))
    ((block_number 4) (max_reconstructed_diff 0)
     (pixels
      ((80 80 80 80 80 80 80 80) (80 80 80 80 80 80 80 80)
       (80 80 80 80 80 80 80 80) (80 80 80 80 80 80 80 80)
       (80 80 80 80 80 80 80 80) (80 80 80 80 80 80 80 80)
       (80 80 80 80 80 80 80 80) (80 80 80 80 80 80 80 80)))
     (comp
      ((x 0) (y 0) (dc_pred 0) (component.identifier 2)
       (coefs
        ((000 000 000 000 000 000 000 000) (000 000 000 000 000 000 000 000)
         (000 000 000 000 000 000 000 000) (000 000 000 000 000 000 000 000)
         (000 000 000 000 000 000 000 000) (000 000 000 000 000 000 000 000)
         (000 000 000 000 000 000 000 000) (000 000 000 000 000 000 000 000)))
       (dequant
        ((000 000 000 000 000 000 000 000) (000 000 000 000 000 000 000 000)
         (000 000 000 000 000 000 000 000) (000 000 000 000 000 000 000 000)
         (000 000 000 000 000 000 000 000) (000 000 000 000 000 000 000 000)
         (000 000 000 000 000 000 000 000) (000 000 000 000 000 000 000 000)))
       (idct
        ((00 00 00 00 00 00 00 00) (00 00 00 00 00 00 00 00)
         (00 00 00 00 00 00 00 00) (00 00 00 00 00 00 00 00)
         (00 00 00 00 00 00 00 00) (00 00 00 00 00 00 00 00)
         (00 00 00 00 00 00 00 00) (00 00 00 00 00 00 00 00)))
       (recon
        ((80 80 80 80 80 80 80 80) (80 80 80 80 80 80 80 80)
         (80 80 80 80 80 80 80 80) (80 80 80 80 80 80 80 80)
         (80 80 80 80 80 80 80 80) (80 80 80 80 80 80 80 80)
         (80 80 80 80 80 80 80 80) (80 80 80 80 80 80 80 80))))))
    ((macroblock 0) (subblock 5) (block_number 5) (x_pos 0) (y_pos 0))
    ((block_number 5) (max_reconstructed_diff 0)
     (pixels
      ((7b 7b 7b 7b 7b 7b 7b 7b) (7b 7b 7b 7b 7b 7b 7b 7b)
       (7b 7b 7b 7b 7b 7b 7b 7b) (7b 7b 7b 7b 7b 7b 7b 7b)
       (7b 7b 7b 7b 7b 7b 7b 7b) (7b 7b 7b 7b 7b 7b 7b 7b)
       (7b 7b 7b 7b 7b 7b 7b 7b) (7b 7b 7b 7b 7b 7b 7b 7b)))
     (comp
      ((x 0) (y 0) (dc_pred -1) (component.identifier 3)
       (coefs
        ((fff 000 000 000 000 000 000 000) (000 000 000 000 000 000 000 000)
         (000 000 000 000 000 000 000 000) (000 000 000 000 000 000 000 000)
         (000 000 000 000 000 000 000 000) (000 000 000 000 000 000 000 000)
         (000 000 000 000 000 000 000 000) (000 000 000 000 000 000 000 000)))
       (dequant
        ((fd5 000 000 000 000 000 000 000) (000 000 000 000 000 000 000 000)
         (000 000 000 000 000 000 000 000) (000 000 000 000 000 000 000 000)
         (000 000 000 000 000 000 000 000) (000 000 000 000 000 000 000 000)
         (000 000 000 000 000 000 000 000) (000 000 000 000 000 000 000 000)))
       (idct
        ((fb fb fb fb fb fb fb fb) (fb fb fb fb fb fb fb fb)
         (fb fb fb fb fb fb fb fb) (fb fb fb fb fb fb fb fb)
         (fb fb fb fb fb fb fb fb) (fb fb fb fb fb fb fb fb)
         (fb fb fb fb fb fb fb fb) (fb fb fb fb fb fb fb fb)))
       (recon
        ((7b 7b 7b 7b 7b 7b 7b 7b) (7b 7b 7b 7b 7b 7b 7b 7b)
         (7b 7b 7b 7b 7b 7b 7b 7b) (7b 7b 7b 7b 7b 7b 7b 7b)
         (7b 7b 7b 7b 7b 7b 7b 7b) (7b 7b 7b 7b 7b 7b 7b 7b)
         (7b 7b 7b 7b 7b 7b 7b 7b) (7b 7b 7b 7b 7b 7b 7b 7b))))))
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────────────────────────┐
    │clock             ││╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥│
    │                  ││╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨│
    │clear             ││╥                                                                                                 │
    │                  ││╨─────────────────────────────────────────────────────────────────────────────────────────────────│
    │start_codeblock_de││       ╥                                  ╥                                 ╥                     │
    │                  ││───────╨──────────────────────────────────╨─────────────────────────────────╨─────────────────────│
    │start_idct        ││       ╥                                  ╥                                 ╥                     │
    │                  ││───────╨──────────────────────────────────╨─────────────────────────────────╨─────────────────────│
    │                  ││╥╥╥───────────────────────────────────────────────────────────────────────────────────────────────│
    │hdr$table_class   ││║║║ 0                                                                                             │
    │                  ││╨╨╨───────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││╥╥────────────────────────────────────────────────────────────────────────────────────────────────│
    │hdr$destination_id││║║ 0                                                                                              │
    │                  ││╨╨────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││╥╥╥╥──────────────────────────────────────────────────────────────────────────────────────────────│
    │code_length_minus1││║║║║ F                                                                                            │
    │                  ││╨╨╨╨──────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││╥╥╥───────────────────────────────────────────────────────────────────────────────────────────────│
    │num_codes_at_lengt││║║║ 00                                                                                            │
    │                  ││╨╨╨───────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││╥╥╥╥──────────────────────────────────────────────────────────────────────────────────────────────│
    │code              ││║║║║ FC00                                                                                         │
    │                  ││╨╨╨╨──────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││╥╥╥───────────────────────────────────────────────────────────────────────────────────────────────│
    │code_base_address ││║║║ 0007                                                                                          │
    │                  ││╨╨╨───────────────────────────────────────────────────────────────────────────────────────────────│
    │code_write        ││╥╥╥╥                                                                                              │
    │                  ││╨╨╨╨──────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││╥╥╥╥──────────────────────────────────────────────────────────────────────────────────────────────│
    │data              ││║║║║ 06                                                                                           │
    │                  ││╨╨╨╨──────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││╥╥╥╥──────────────────────────────────────────────────────────────────────────────────────────────│
    │data_address      ││║║║║ 0006                                                                                         │
    │                  ││╨╨╨╨──────────────────────────────────────────────────────────────────────────────────────────────│
    │data_write        ││╥╥╥╥                                                                                              │
    │                  ││╨╨╨╨──────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││───╥─╥────────────────────────────────────────────────────────────────────────────────────────────│
    │table_identifier  ││ 0 ║ ║ 0                                                                                          │
    │                  ││───╨─╨────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││───╥─╥╥╥──────────────────────────────────────────────────────────────────────────────────────────│
    │element           ││ 0.║ ║║║ 00F8                                                                                     │
    │                  ││───╨─╨╨╨──────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││───╥╥╥╥╥──────────────────────────────────────────────────────────────────────────────────────────│
    │element_address   ││ 00║║║║║ 3F                                                                                       │
    │                  ││───╨╨╨╨╨──────────────────────────────────────────────────────────────────────────────────────────│
    │element_write     ││   ╥───╥                                                                                          │
    │                  ││───╨   ╨──────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │dc_table_select   ││ 0                                                                                                │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │qnt_table_select  ││ 0                                                                                                │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││──────────────────────────────────────────┬───────────────────────────────────────────────────────│
    │dc_pred_in        ││ 000                                      │014                                                    │
    │                  ││──────────────────────────────────────────┴───────────────────────────────────────────────────────│
    │                  ││───────╥╥─────────────────────────────────╥─────────────────────────────────╥─────────────────────│
    │jpeg              ││ 0000  ║║ 6D0E                            ║ 1ACE                            ║ EDC7                │
    │                  ││───────╨╨─────────────────────────────────╨─────────────────────────────────╨─────────────────────│
    │jpeg_valid        ││       ╥──────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││───────╨                                                                                          │
    │                  ││───────╥╥╥────────────────────────────────╥╥╥───────────────────────────────╥╥╥───────────────────│
    │pixel_read_address││ 00    ║║║ 3F                             ║║║ 3F                            ║║║ 3F                │
    │                  ││───────╨╨╨────────────────────────────────╨╨╨───────────────────────────────╨╨╨───────────────────│
    │pixel_read_enable ││       ╥─╥                                ╥─╥                               ╥─╥                   │
    │                  ││───────╨ ╨────────────────────────────────╨ ╨───────────────────────────────╨ ╨───────────────────│
    │                  ││────────────────────────────────────────────────────────────────────────────╥╥╥───────────────────│
    │pixel             ││ 80                                                                         ║║║ EB                │
    │                  ││────────────────────────────────────────────────────────────────────────────╨╨╨───────────────────│
    │                  ││───────╥────────────────────────────────────────────────────────────────────╥─────────────────────│
    │dc_pred_out       ││ 000   ║ 014                                                                ║ 00E                 │
    │                  ││───────╨────────────────────────────────────────────────────────────────────╨─────────────────────│
    │jpeg_ready        ││       ╥╥                                 ╥                                 ╥                     │
    └──────────────────┘└──────────────────────────────────────────────────────────────────────────────────────────────────┘ |}]
;;
