open! Core
open! Hardcaml
open! Hardcaml_waveterm
module Writer = Hardcaml_jpeg.Bitstream_writer
module T = Writer.Make (Bits)

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

let%expect_test "simulation" =
  let sim =
    Sim.create ~config:Cyclesim.Config.trace_all (Writer.create (Scope.create ()))
  in
  let waves, sim = Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  inputs.clocking.clear := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clocking.clear := Bits.gnd;
  Cyclesim.cycle sim;
  inputs.bits := Bits.of_int ~width:16 0xf;
  inputs.num_bits := Bits.of_int ~width:5 8;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Waveform.print waves;
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clock          ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌──│
    │               ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘  │
    │clear          ││────────┐                                          │
    │               ││        └───────────────────────────────           │
    │               ││────────────────┬───────────────────────           │
    │bits           ││ 0000           │000F                              │
    │               ││────────────────┴───────────────────────           │
    │               ││────────────────┬───────────────────────           │
    │num_bits       ││ 00             │08                                │
    │               ││────────────────┴───────────────────────           │
    │               ││────────────────────────────────────────           │
    │bits_out       ││ 0000                                              │
    │               ││────────────────────────────────────────           │
    │bits_valid     ││                                ┌───────           │
    │               ││────────────────────────────────┘                  │
    │               ││────────────────────────┬───────┬───────           │
    │BUFFER         ││ 00000000               │000000.│00000F.           │
    │               ││────────────────────────┴───────┴───────           │
    └───────────────┘└───────────────────────────────────────────────────┘ |}]
;;
