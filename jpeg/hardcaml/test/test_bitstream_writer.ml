open! Core
open! Hardcaml
open! Hardcaml_waveterm
module Writer = Hardcaml_jpeg.Bitstream_writer
module T = Writer.For_testing.Make (Bits)

let%expect_test "insert at bottom" =
  let count = ref 0 in
  let buffer = ref (Bits.zero 32) in
  let insert ~data_in ~bits =
    buffer
      := T.insert_at_bottom
           ~buffer:!buffer
           ~data_in:(Bits.of_int ~width:16 data_in)
           ~bits:(Bits.of_int ~width:5 bits);
    count := !count + bits
  in
  insert ~data_in:0xff ~bits:8;
  print_s [%message (!buffer : Bits.t)];
  [%expect {| (!buffer 00000000000000000000000011111111) |}];
  insert ~data_in:0xaa ~bits:8;
  print_s [%message (!buffer : Bits.t)];
  [%expect {| (!buffer 00000000000000001111111110101010) |}];
  insert ~data_in:0xf ~bits:4;
  print_s [%message (!buffer : Bits.t)];
  [%expect {| (!buffer 00000000000011111111101010101111) |}];
  insert ~data_in:0x0 ~bits:1;
  print_s [%message (!buffer : Bits.t)];
  [%expect {| (!buffer 00000000000111111111010101011110) |}];
  insert ~data_in:0x1 ~bits:1;
  print_s [%message (!buffer : Bits.t)];
  [%expect {| (!buffer 00000000001111111110101010111101) |}]
;;

module Sim = Cyclesim.With_interface (Writer.I) (Writer.O)

let create_sim () =
  let sim =
    Sim.create ~config:Cyclesim.Config.trace_all (Writer.create (Scope.create ()))
  in
  let waves, sim = Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  inputs.clocking.clear := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clocking.clear := Bits.gnd;
  Cyclesim.cycle sim;
  sim, waves, inputs, outputs
;;

let create_sim_with_put () =
  let sim, waves, inputs, _ = create_sim () in
  let put num bits =
    inputs.bits := Bits.of_int ~width:16 bits;
    inputs.num_bits := Bits.of_int ~width:5 num;
    Cyclesim.cycle sim
  in
  sim, waves, put
;;

let%expect_test "simulation - 4 bits" =
  let sim, waves, put = create_sim_with_put () in
  for i = 0 to 3 do
    put 4 i
  done;
  put 0 0;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Waveform.print ~display_height:23 ~display_width:80 ~wave_width:2 waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────┐
    │clock             ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐│
    │                  ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └│
    │clear             ││──────┐                                                   │
    │                  ││      └───────────────────────────────────────────────    │
    │                  ││──────────────────┬─────┬─────┬─────┬─────────────────    │
    │bits              ││ 0000             │0001 │0002 │0003 │0000                 │
    │                  ││──────────────────┴─────┴─────┴─────┴─────────────────    │
    │                  ││────────────┬───────────────────────┬─────────────────    │
    │num_bits          ││ 00         │04                     │00                   │
    │                  ││────────────┴───────────────────────┴─────────────────    │
    │                  ││────────────────────────────────────┬─────┬───────────    │
    │bits_out          ││ 0000                               │0123 │0000           │
    │                  ││────────────────────────────────────┴─────┴───────────    │
    │bits_valid        ││                                    ┌─────┐               │
    │                  ││────────────────────────────────────┘     └───────────    │
    │                  ││────────────────────────┬─────┬─────┬─────────────────    │
    │BUFFER            ││ 00000000               │0000.│0000.│00000123             │
    │                  ││────────────────────────┴─────┴─────┴─────────────────    │
    │                  ││──────────────────┬─────┬─────┬─────┬─────┬───────────    │
    │COUNT             ││ 00               │04   │08   │0C   │10   │00             │
    │                  ││──────────────────┴─────┴─────┴─────┴─────┴───────────    │
    └──────────────────┘└──────────────────────────────────────────────────────────┘ |}]
;;

let%expect_test "simulation - 8 bits" =
  let sim, waves, put = create_sim_with_put () in
  for i = 0 to 3 do
    put 8 i
  done;
  put 0 0;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Waveform.print ~display_height:23 ~display_width:80 ~wave_width:2 waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────┐
    │clock             ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐│
    │                  ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └│
    │clear             ││──────┐                                                   │
    │                  ││      └───────────────────────────────────────────────    │
    │                  ││──────────────────┬─────┬─────┬─────┬─────────────────    │
    │bits              ││ 0000             │0001 │0002 │0003 │0000                 │
    │                  ││──────────────────┴─────┴─────┴─────┴─────────────────    │
    │                  ││────────────┬───────────────────────┬─────────────────    │
    │num_bits          ││ 00         │08                     │00                   │
    │                  ││────────────┴───────────────────────┴─────────────────    │
    │                  ││────────────────────────┬─────┬─────┬─────┬───────────    │
    │bits_out          ││ 0000                   │0001 │0000 │0203 │0001           │
    │                  ││────────────────────────┴─────┴─────┴─────┴───────────    │
    │bits_valid        ││                        ┌─────┐     ┌─────┐               │
    │                  ││────────────────────────┘     └─────┘     └───────────    │
    │                  ││────────────────────────┬─────┬─────┬─────────────────    │
    │BUFFER            ││ 00000000               │0000.│0000.│00010203             │
    │                  ││────────────────────────┴─────┴─────┴─────────────────    │
    │                  ││──────────────────┬─────┬─────┬─────┬─────┬───────────    │
    │COUNT             ││ 00               │08   │10   │08   │10   │00             │
    │                  ││──────────────────┴─────┴─────┴─────┴─────┴───────────    │
    └──────────────────┘└──────────────────────────────────────────────────────────┘ |}]
;;

let random_test len =
  let sim, _, inputs, outputs = create_sim () in
  (* Random inputs *)
  let bits_in = Bits.random ~width:len in
  let bits_out = ref [] in
  (* Feed the input a random number of bits at a time while collecting the output. *)
  let cycle () =
    if Bits.to_bool !(outputs.bits_valid)
    then bits_out := !(outputs.bits_out) :: !bits_out;
    Cyclesim.cycle sim
  in
  let bits_in_shift = ref bits_in in
  while not (Bits.is_empty !bits_in_shift) do
    let width_left = Bits.width !bits_in_shift in
    let nbits = Random.int (1 + min 16 (Bits.width !bits_in_shift)) in
    if nbits <> 0
    then (
      inputs.bits := Bits.uresize (Bits.sel_top !bits_in_shift nbits) 16;
      bits_in_shift
        := if width_left = nbits then Bits.empty else Bits.drop_top !bits_in_shift nbits);
    inputs.num_bits := Bits.of_int ~width:5 nbits;
    cycle ()
  done;
  (* flush output *)
  let rounded_width = Int.round_up ~to_multiple_of:16 len in
  let bits_left = rounded_width - len in
  inputs.bits := Bits.zero 16;
  inputs.num_bits := Bits.of_int ~width:5 bits_left;
  cycle ();
  cycle ();
  cycle ();
  (* Check result *)
  let bits_out = Bits.concat_msb (List.rev !bits_out) in
  if Bits.width bits_out <> rounded_width
  then
    raise_s
      [%message
        "output not expected length" (rounded_width : int) (Bits.width bits_out : int)];
  let bits_out = Bits.sel_top bits_out len in
  if not (Bits.equal bits_in bits_out)
  then raise_s [%message "mismatch" (bits_in : Bits.t) (bits_out : Bits.t)]
;;

let%expect_test "small tests" =
  for _ = 1 to 100 do
    random_test 100
  done;
  [%expect{||}]
;;

let%expect_test "big test" =
  random_test 10_000;
  [%expect{||}]
;;
