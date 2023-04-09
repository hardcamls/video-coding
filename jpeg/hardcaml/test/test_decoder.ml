open! Core
open! Hardcaml
open Hardcaml_jpeg
open Hardcaml_jpeg_model
open! Hardcaml_waveterm

module Decoder = struct
  module Decoder = Decoder.Core
  module Reader = Util.Super_simple_bitstream_reader

  module I = struct
    type 'a t =
      { clocking : 'a Clocking.t
      ; start : 'a
      ; markers : 'a Vld.Core.All_markers.t
      ; pixel_read_address : 'a [@bits 6]
      ; pixel_read_enable : 'a
      ; output_done : 'a
      }
    [@@deriving sexp_of, hardcaml ~rtlmangle:"$"]
  end

  module O = Decoder.O

  let create ~bits scope (i : _ I.t) =
    let reader = Reader.O.Of_signal.wires () in
    let decoder =
      Decoder.hierarchical
        scope
        { Decoder.I.clocking = i.clocking
        ; start = i.start
        ; markers = i.markers
        ; bits = reader.bits
        ; pixel_read_address = i.pixel_read_address
        ; pixel_read_enable = i.pixel_read_enable
        ; output_done = i.output_done
        }
    in
    Reader.O.Of_signal.assign
      reader
      (Reader.create
         ~bits
         scope
         { Reader.I.clocking = i.clocking; read_bits = decoder.read_bits });
    decoder
  ;;
end

module Sim = Cyclesim.With_interface (Decoder.I) (Decoder.O)

let ( <--. ) a b = a := Bits.of_int ~width:(Bits.width !a) b

let reconstruct block_number frame pixels =
  let macroblock = block_number / 6 in
  let subblock = block_number % 6 in
  let width = Frame.width frame in
  let y_pos = macroblock / (width / 16) in
  let x_pos = macroblock % (width / 16) in
  print_s
    [%message
      (macroblock : int) (subblock : int) (width : int) (x_pos : int) (y_pos : int)];
  let copy plane x_pos y_pos =
    for y = 0 to 7 do
      for x = 0 to 7 do
        Plane.(plane.![x_pos + x, y_pos + y] <- Char.of_int_exn pixels.(x + (y * 8)))
      done
    done
  in
  match subblock with
  | 0 -> copy (Frame.y frame) (x_pos * 16) (y_pos * 16)
  | 1 -> copy (Frame.y frame) ((x_pos * 16) + 8) (y_pos * 16)
  | 2 -> copy (Frame.y frame) (x_pos * 16) ((y_pos * 16) + 8)
  | 3 -> copy (Frame.y frame) ((x_pos * 16) + 8) ((y_pos * 16) + 8)
  | 4 -> copy (Frame.u frame) (x_pos * 8) (y_pos * 8)
  | 5 -> copy (Frame.v frame) (x_pos * 8) (y_pos * 8)
  | _ -> failwith ""
;;

let max_reconstructed_diff pixels recon =
  Array.fold2_exn pixels recon ~init:0 ~f:(fun acc a b ->
      let diff = Int.abs (a - b) in
      if diff > acc then acc else diff)
;;

let test ?(waves = true) ?(error_tolerance = 1) ?num_blocks_to_decode jpeg =
  let bits = Util.load_jpeg_file jpeg in
  let headers = Model.Header.decode bits in
  let decoder = Model.init headers bits in
  let entropy_coded_bits =
    Model.entropy_coded_bits decoder |> Bitstream_reader.From_string.get_buffer
  in
  let model = Model.For_testing.Sequenced.decode decoder in
  (* let headers, entropy_bits = Util.headers_and_entropy_coded_segment jpeg in *)
  let width = (Model.Header.frame headers |> Option.value_exn).width in
  let height = (Model.Header.frame headers |> Option.value_exn).height in
  print_s [%message (width : int) (height : int)];
  let frame = Frame.create ~chroma_subsampling:C420 ~width ~height in
  let sim =
    Sim.create
      ~config:{ Cyclesim.Config.trace_all with deduplicate_signals = false }
      (Decoder.create
         ~bits:entropy_coded_bits
         (Scope.create ~auto_label_hierarchical_ports:true ~flatten_design:true ()))
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
  inputs.output_done := Bits.vdd;
  inputs.clocking.clear := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clocking.clear := Bits.gnd;
  (* configure core *)
  let huffman_tables = Model.Header.huffman_tables headers in
  Util.load_huffman_tables
    ~cycle:(fun () -> Cyclesim.cycle sim)
    inputs.markers.dht
    huffman_tables;
  let quant_tables = Model.Header.quant_tables headers in
  Util.load_quant_tables
    ~cycle:(fun () -> Cyclesim.cycle sim)
    inputs.markers.dqt
    quant_tables;
  (* run core *)
  inputs.start := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.start := Bits.gnd;
  let output_a_block () =
    let num_cycles = ref 0 in
    let cycle () =
      if !num_cycles > 2_000 then raise_s [%message "Took too long to decode block"];
      Cyclesim.cycle sim;
      Int.incr num_cycles
    in
    let pixels = ref [] in
    let add_pixel () = pixels := Bits.to_int !(outputs.pixel) :: !pixels in
    while not (Bits.to_bool !(outputs.start_output)) do
      cycle ()
    done;
    inputs.output_done := Bits.gnd;
    cycle ();
    inputs.pixel_read_enable := Bits.vdd;
    for i = 0 to 63 do
      inputs.pixel_read_address <--. i;
      cycle ();
      add_pixel ()
    done;
    inputs.pixel_read_enable := Bits.gnd;
    cycle ();
    inputs.output_done := Bits.vdd;
    cycle ();
    List.rev !pixels |> Array.of_list
  in
  let rec decode_and_compare_with_model model block_number =
    match num_blocks_to_decode with
    | Some num_blocks_to_decode when block_number >= num_blocks_to_decode ->
      (* finish early *) ()
    | Some _ | None ->
      (match Sequence.hd model with
      | None -> () (* finished decoding *)
      | Some comp ->
        (* Run simulator *)
        let pixels = output_a_block () in
        reconstruct block_number frame pixels;
        if max_reconstructed_diff pixels (Model.Component.recon comp) >= error_tolerance
        then
          print_s
            [%message
              (block_number : int)
                (pixels : Model.Component.Summary.pixel_block)
                (comp : Model.Component.Summary.t)];
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

let%expect_test "test decoder" =
  let _, waves = test ~waves:true ~num_blocks_to_decode:6 "Mouse480.jpg" in
  Option.iter
    waves
    ~f:(Waveform.print ~display_height:50 ~display_width:120 ~wave_width:2 ~start_cycle:0);
  [%expect
    {|
    ((width 480) (height 320))
    ((macroblock 0) (subblock 0) (width 480) (x_pos 0) (y_pos 0))
    ((macroblock 0) (subblock 1) (width 480) (x_pos 0) (y_pos 0))
    ((macroblock 0) (subblock 2) (width 480) (x_pos 0) (y_pos 0))
    ((macroblock 0) (subblock 3) (width 480) (x_pos 0) (y_pos 0))
    ((macroblock 0) (subblock 4) (width 480) (x_pos 0) (y_pos 0))
    ((macroblock 0) (subblock 5) (width 480) (x_pos 0) (y_pos 0))
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────────────────────────┐
    │clocking$clear    ││──────┐                                                                                           │
    │                  ││      └───────────────────────────────────────────────────────────────────────────────────────────│
    │clocking$clock    ││                                                                                                  │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││────────────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─│
    │markers$dht$code  ││ 0000       │0002 │0006 │000E │001C │0038 │0070 │00E0 │01C0 │0380 │0700 │0E00 │1C00 │3800 │7000 │E│
    │                  ││────────────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─│
    │                  ││────────────┬─────┬─────┬─────────────────────────────────────────────────────────────────────────│
    │markers$dht$code_b││ 0000       │0001 │0002 │0003                                                                     │
    │                  ││────────────┴─────┴─────┴─────────────────────────────────────────────────────────────────────────│
    │                  ││────────────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─│
    │markers$dht$code_l││ 0          │1    │2    │3    │4    │5    │6    │7    │8    │9    │A    │B    │C    │D    │E    │F│
    │                  ││────────────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─│
    │markers$dht$code_w││      ┌───────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││──────┘                                                                                           │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │markers$dht$data  ││ 00                                                                                               │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │markers$dht$data_a││ 0000                                                                                             │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │markers$dht$data_w││                                                                                                  │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││──────┬───────────────────────────────────────────────────────────────────────────────────────────│
    │markers$dht$hdr$de││ 0    │1                                                                                          │
    │                  ││──────┴───────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││──────┬───────────────────────────────────────────────────────────────────────────────────────────│
    │markers$dht$hdr$ta││ 0    │1                                                                                          │
    │                  ││──────┴───────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││──────┬─────────────────┬─────────────────────────────────────────────────────────────────────────│
    │markers$dht$num_co││ 00   │01               │00                                                                       │
    │                  ││──────┴─────────────────┴─────────────────────────────────────────────────────────────────────────│
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │markers$dqt$elemen││ 0000                                                                                             │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │markers$dqt$elemen││ 00                                                                                               │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │markers$dqt$elemen││                                                                                                  │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │markers$dqt$table_││ 0                                                                                                │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │output_done       ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││                                                                                                  │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │pixel_read_address││ 00                                                                                               │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    └──────────────────┘└──────────────────────────────────────────────────────────────────────────────────────────────────┘ |}]
;;
