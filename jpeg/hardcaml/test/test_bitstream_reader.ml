open Core
open Hardcaml
open Hardcaml_waveterm
module Clocking = Hardcaml_jpeg.Clocking
module Reader = Hardcaml_jpeg.Bitstream_reader
module Sim = Cyclesim.With_interface (Reader.I) (Reader.O)

let test () =
  let scope = Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true () in
  let sim = Sim.create ~config:Cyclesim.Config.trace_all (Reader.create scope) in
  let waves, sim = Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  let _outputs = Cyclesim.outputs sim in
  let outputs_before = Cyclesim.outputs ~clock_edge:Before sim in
  inputs.clocking.clear := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clocking.clear := Bits.gnd;
  inputs.jpeg_valid := Bits.vdd;
  let pos = ref 0 in
  let cycle () =
    Cyclesim.cycle sim;
    if Bits.to_bool !(outputs_before.jpeg_ready)
    then (
      inputs.jpeg_in := Bits.of_int ~width:16 (0x1234 + (0x4444 * !pos));
      Int.incr pos)
  in
  for _ = 0 to 4 do
    inputs.read_entropy_bits := Bits.of_int ~width:5 8;
    cycle ()
  done;
  inputs.read_entropy_bits := Bits.of_int ~width:5 16;
  cycle ();
  inputs.read_entropy_bits := Bits.of_int ~width:5 8;
  cycle ();
  inputs.read_entropy_bits := Bits.of_int ~width:5 16;
  cycle ();
  inputs.read_entropy_bits := Bits.of_int ~width:5 8;
  cycle ();
  inputs.read_entropy_bits := Bits.of_int ~width:5 16;
  cycle ();
  waves
;;

let%expect_test "buffer" =
  let waves = test () in
  Waveform.print waves ~display_height:40 ~display_width:120 ~wave_width:3 ~start_cycle:0;
  [%expect
    {|
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────────────────────────┐
    │clock             ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌─│
    │                  ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘ │
    │clear             ││────────┐                                                                                         │
    │                  ││        └───────────────────────────────────────────────────────────────────────────────          │
    │                  ││────────────────────────┬───────────────┬───────────────┬───────┬───────┬───────────────          │
    │jpeg_in           ││ 0000                   │1234           │5678           │9ABC   │DF00   │2344                     │
    │                  ││────────────────────────┴───────────────┴───────────────┴───────┴───────┴───────────────          │
    │jpeg_valid        ││        ┌───────────────────────────────────────────────────────────────────────────────          │
    │                  ││────────┘                                                                                         │
    │                  ││────────┬───────────────────────────────────────┬───────┬───────┬───────┬───────┬───────          │
    │read_entropy_bits ││ 00     │08                                     │10     │08     │10     │08     │10               │
    │                  ││────────┴───────────────────────────────────────┴───────┴───────┴───────┴───────┴───────          │
    │                  ││────────────────────────────────────────────────────────┬───────┬───────┬───────┬───────          │
    │bits              ││ 0000                                                   │1234   │3456   │789A   │9ABC             │
    │                  ││────────────────────────────────────────────────────────┴───────┴───────┴───────┴───────          │
    │bits_valid        ││────────────────────────────────────────────────────────────────────────────────────────          │
    │                  ││                                                                                                  │
    │jpeg_ready        ││                ┌───────┐       ┌───────┐       ┌───────────────────────┐       ┌───────          │
    │                  ││────────────────┘       └───────┘       └───────┘                       └───────┘                 │
    │STATE             ││                                                                                                  │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────          │
    │                  ││────────────────────────────────────────┬───────┬───────┬───────┬───────┬───────┬───────          │
    │buffer            ││ 0000000000                             │000000.│000012.│123456.│345678.│789ABC.│9ABCDF.          │
    │                  ││────────────────────────────────────────┴───────┴───────┴───────┴───────┴───────┴───────          │
    │                  ││────────────────────────────────┬───────┬───────┬───────┬───────┬───────┬───────┬───────          │
    │buffer_next       ││ 0000000000                     │000000.│000012.│123456.│345678.│789ABC.│9ABCDF.│DF0023.          │
    │                  ││────────────────────────────────┴───────┴───────┴───────┴───────┴───────┴───────┴───────          │
    │gnd               ││                                                                                                  │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────          │
    │one_byte_shifted  ││                ┌───────┐       ┌───────┐       ┌───────────────┐               ┌───────          │
    │                  ││────────────────┘       └───────┘       └───────┘               └───────────────┘                 │
    │one_byte_shifted_n││        ┌───────┐       ┌───────┐       ┌───────────────┐               ┌───────────────          │
    │                  ││────────┘       └───────┘       └───────┘               └───────────────┘                         │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────          │
    │shift_offset      ││ 0                                                                                                │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────          │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────          │
    │shift_offset_next ││ 00                                                                                               │
    └──────────────────┘└──────────────────────────────────────────────────────────────────────────────────────────────────┘ |}]
;;

let test ?(waves = false) data bits =
  let scope = Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true () in
  let sim = Sim.create ~config:Cyclesim.Config.trace_all (Reader.create scope) in
  let waves, sim =
    if waves
    then (
      let waves, sim = Waveform.create sim in
      Some waves, sim)
    else None, sim
  in
  let inputs = Cyclesim.inputs sim in
  let _outputs = Cyclesim.outputs sim in
  let outputs_before = Cyclesim.outputs ~clock_edge:Before sim in
  inputs.clocking.clear := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clocking.clear := Bits.gnd;
  inputs.jpeg_valid := Bits.vdd;
  let pos = ref 0 in
  let cycle () =
    Cyclesim.cycle sim;
    if Bits.to_bool !(outputs_before.jpeg_ready)
    then (
      inputs.jpeg_in
        := Bits.of_int
             ~width:16
             (try data.(!pos) with
             | _ -> 0);
      Int.incr pos)
  in
  (* flush *)
  for _ = 0 to 6 do
    inputs.read_entropy_bits := Bits.of_int ~width:5 8;
    cycle ()
  done;
  let cbits =
    Hardcaml_jpeg_model.Bitstream_reader.From_string.create
      (String.of_char_list
         (List.init (Array.length data) ~f:(fun i ->
              [ Char.of_int_exn (data.(i) lsr 8); Char.of_int_exn (data.(i) land 0xff) ])
         |> List.concat))
  in
  let check bits =
    if bits <> 0
    then (
      let obits = Bits.to_int !(outputs_before.bits) in
      let hbits = obits lsr (16 - bits) in
      let cbits = Hardcaml_jpeg_model.Bitstream_reader.From_string.get cbits bits in
      if hbits <> cbits
      then
        print_s
          [%message
            (bits : int) (cbits : Int.Hex.t) (obits : Int.Hex.t) (hbits : Int.Hex.t)])
  in
  Array.iter bits ~f:(fun bits ->
      inputs.read_entropy_bits := Bits.of_int ~width:5 bits;
      cycle ();
      check bits);
  waves
;;

let%expect_test "..." =
  let waves =
    test ~waves:true [| 0x0123; 0x4567; 0x89ab; 0xcdef |] [| 2; 3; 7; 12; 13; 1; 1; 9 |]
  in
  Option.iter
    waves
    ~f:(Waveform.print ~display_height:40 ~display_width:120 ~wave_width:2 ~start_cycle:0);
  [%expect
    {|
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────────────────────────┐
    │clock             ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌─│
    │                  ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘ │
    │clear             ││──────┐                                                                                           │
    │                  ││      └─────────────────────────────────────────────────────────────────────────────────────────  │
    │                  ││──────────────────┬───────────┬───────────┬───────────────────────┬─────┬───────────────────────  │
    │jpeg_in           ││ 0000             │0123       │4567       │89AB                   │CDEF │0000                     │
    │                  ││──────────────────┴───────────┴───────────┴───────────────────────┴─────┴───────────────────────  │
    │jpeg_valid        ││      ┌─────────────────────────────────────────────────────────────────────────────────────────  │
    │                  ││──────┘                                                                                           │
    │                  ││──────┬─────────────────────────────────────────┬─────┬─────┬─────┬─────┬─────┬───────────┬─────  │
    │read_entropy_bits ││ 00   │08                                       │02   │03   │07   │0C   │0D   │01         │09     │
    │                  ││──────┴─────────────────────────────────────────┴─────┴─────┴─────┴─────┴─────┴───────────┴─────  │
    │                  ││──────────────────────────────────────────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────  │
    │bits              ││ 0000                                     │0001 │0123 │048D │2468 │3456 │6789 │3579 │6AF3 │D5E6   │
    │                  ││──────────────────────────────────────────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────  │
    │bits_valid        ││────────────────────────────────────────────────────────────────────────────────────────────────  │
    │                  ││                                                                                                  │
    │jpeg_ready        ││            ┌─────┐     ┌─────┐     ┌─────┐                 ┌───────────┐                 ┌─────  │
    │                  ││────────────┘     └─────┘     └─────┘     └─────────────────┘           └─────────────────┘       │
    │STATE             ││                                                                                                  │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────  │
    │                  ││──────────────────────────────┬─────┬─────┬─────┬─────────────────┬─────┬─────┬─────────────────  │
    │buffer            ││ 0000000000                   │0000.│0000.│0001.│0123456700       │2345.│6789.│89ABCDEF00         │
    │                  ││──────────────────────────────┴─────┴─────┴─────┴─────────────────┴─────┴─────┴─────────────────  │
    │                  ││────────────────────────┬─────┬─────┬─────┬─────────────────┬─────┬─────┬─────────────────┬─────  │
    │buffer_next       ││ 0000000000             │0000.│0000.│0001.│0123456700       │2345.│6789.│89ABCDEF00       │CDEF.  │
    │                  ││────────────────────────┴─────┴─────┴─────┴─────────────────┴─────┴─────┴─────────────────┴─────  │
    │gnd               ││                                                                                                  │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────  │
    │one_byte_shifted  ││            ┌─────┐     ┌─────┐     ┌─────┐     ┌─────────────────┐           ┌─────────────────  │
    │                  ││────────────┘     └─────┘     └─────┘     └─────┘                 └───────────┘                   │
    │one_byte_shifted_n││      ┌─────┐     ┌─────┐     ┌─────┐     ┌─────────────────┐           ┌───────────────────────  │
    │                  ││──────┘     └─────┘     └─────┘     └─────┘                 └───────────┘                         │
    │                  ││──────────────────────────────────────────────────────┬─────┬─────┬─────┬─────┬─────┬─────┬─────  │
    │shift_offset      ││ 0                                                    │2    │5    │4    │0    │5    │6    │7      │
    │                  ││──────────────────────────────────────────────────────┴─────┴─────┴─────┴─────┴─────┴─────┴─────  │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────  │
    │shift_offset_next ││ 00                                                                                               │
    └──────────────────┘└──────────────────────────────────────────────────────────────────────────────────────────────────┘ |}]
;;

let%expect_test "..." =
  let data = Array.init 10000 ~f:(fun _ -> Random.int (1 lsl 16)) in
  let bits = Array.init 10000 ~f:(fun _ -> Random.int 17) in
  ignore (test ~waves:false data bits : Waveform.t option);
  [%expect {||}]
;;
