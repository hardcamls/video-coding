open! Core
open! Hardcaml
open Hardcaml_jpeg
open! Hardcaml_waveterm
module Sim = Cyclesim.With_interface (Decoder.I) (Decoder.O)

include struct
  open Hardcaml_jpeg_model
  module Frame = Frame
  module Model = Decoder
  module Reader = Bitstream_reader
  module Sexp_util = Util
end
(* XX decoder bug occurs at block=1875 with large reconstructed diff (38).  
   Assuming a bytestream problem as this doesn't happen in the accelerator design. 
*)

let ( <--. ) a b = a := Bits.of_int ~width:(Bits.width !a) b

let max_reconstructed_diff pixels recon =
  Array.fold2_exn pixels recon ~init:0 ~f:(fun acc a b ->
      let diff = Int.abs (a - b) in
      if diff < acc then acc else diff)
;;

let test ?(waves = true) ?(error_tolerance = 2) ?num_blocks_to_decode jpeg =
  let bits = Util.load_jpeg_file jpeg in
  let headers = Model.Header.decode bits in
  let decoder = Model.init headers bits in
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
  let outputs_before = Cyclesim.outputs ~clock_edge:Before sim in
  inputs.output_done := Bits.vdd;
  inputs.clocking.clear := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clocking.clear := Bits.gnd;
  (* run core *)
  inputs.start := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.start := Bits.gnd;
  let jpeg_pos = ref 0 in
  let bits = Reader.From_string.get_buffer bits in
  let output_a_block () =
    let num_cycles = ref 0 in
    let cycle () =
      if !num_cycles > 3_000 then raise_s [%message "Took too long to decode block"];
      Cyclesim.cycle sim;
      if Bits.to_bool !(outputs_before.jpeg_ready)
      then (
        (inputs.jpeg_valid := Bits.vdd;
         inputs.jpeg
           := try Bits.of_char bits.[!jpeg_pos] with
              | _ -> Bits.zero 8);
        Int.incr jpeg_pos);
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
        (* print_s [%message (pixels : Int.Hex.t array)]; *)
        Util.reconstruct ~block_number frame pixels;
        let max_reconstructed_diff =
          max_reconstructed_diff pixels (Model.Component.recon comp)
        in
        if max_reconstructed_diff >= error_tolerance
        then
          print_s
            [%message
              (block_number : int)
                (max_reconstructed_diff : int)
                (pixels : Sexp_util.pixel_block)
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
  let _, waves =
    test ~waves:true ~num_blocks_to_decode:6 ~error_tolerance:2 "Mouse480.jpg"
  in
  Option.iter
    waves
    ~f:(Waveform.print ~display_height:50 ~display_width:120 ~wave_width:2 ~start_cycle:0);
  [%expect
    {|
    ((width 480) (height 320))
    ((macroblock 0) (subblock 0) (block_number 0) (x_pos 0) (y_pos 0))
    ((macroblock 0) (subblock 1) (block_number 1) (x_pos 0) (y_pos 0))
    ((macroblock 0) (subblock 2) (block_number 2) (x_pos 0) (y_pos 0))
    ((macroblock 0) (subblock 3) (block_number 3) (x_pos 0) (y_pos 0))
    ((macroblock 0) (subblock 4) (block_number 4) (x_pos 0) (y_pos 0))
    ((macroblock 0) (subblock 5) (block_number 5) (x_pos 0) (y_pos 0))
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────────────────────────┐
    │clock             ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌─│
    │                  ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘ │
    │clear             ││──────┐                                                                                           │
    │                  ││      └───────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││──────────────────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────────────│
    │jpeg              ││ 00               │FF   │D8   │FF   │E0   │00   │10   │4A   │46   │49   │46   │00   │01           │
    │                  ││──────────────────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────────────│
    │jpeg_valid        ││                  ┌───────────────────────────────────────────────────────────────────────────────│
    │                  ││──────────────────┘                                                                               │
    │output_done       ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││                                                                                                  │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │pixel_read_address││ 00                                                                                               │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │pixel_read_enable ││                                                                                                  │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │start             ││      ┌─────┐                                                                                     │
    │                  ││──────┘     └─────────────────────────────────────────────────────────────────────────────────────│
    │jpeg_ready        ││            ┌─────────────────────────────────────────────────────────────────────────────────────│
    │                  ││────────────┘                                                                                     │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │pixel             ││ 80                                                                                               │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │start_output      ││                                                                                                  │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │bsread$STATE      ││ 0                                                                                                │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │bsread$buffer     ││ 0000000000                                                                                       │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │bsread$buffer_next││ 0000000000                                                                                       │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │bsread$i$clear    ││──────┐                                                                                           │
    │                  ││      └───────────────────────────────────────────────────────────────────────────────────────────│
    │bsread$i$clock    ││                                                                                                  │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │bsread$i$jpeg_in  ││ 0000                                                                                             │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │bsread$i$jpeg_vali││                                                                                                  │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │bsread$i$read_bits││ 00                                                                                               │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │bsread$i$start    ││                                                                                                  │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    └──────────────────┘└──────────────────────────────────────────────────────────────────────────────────────────────────┘ |}]
;;
