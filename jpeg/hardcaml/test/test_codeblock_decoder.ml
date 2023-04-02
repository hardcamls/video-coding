open Core
open Hardcaml
open Hardcaml_jpeg
open Hardcaml_waveterm
module Decoder = Codeblock_decoder
module Sim = Cyclesim.With_interface (Decoder.I) (Decoder.O)

(* To run this testbench we need to
   
  1. Pull out the headers, and program them into the hardware
  2. Find Sos, and extract the entropy coded bits
  3. Remove the markers bytes
*)

let test ?(waves = false) () =
  let sim = Sim.create (Decoder.create (Scope.create ~flatten_design:true ())) in
  let waves, sim =
    if waves
    then (
      let waves, sim = Waveform.create sim in
      Some waves, sim)
    else None, sim
  in
  let inputs = Cyclesim.inputs sim in
  inputs.clocking.clear := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clocking.clear := Bits.gnd;
  Cyclesim.cycle sim;
  waves
;;

let%expect_test "test" =
  Option.iter
    (test ~waves:true ())
    ~f:(Waveform.print ~display_width:100 ~display_height:40 ~wave_width:2);
  [%expect
    {|
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────┐
    │clock             ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  │
    │                  ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──│
    │clear             ││──────┐                                                                       │
    │                  ││      └─────                                                                  │
    │                  ││────────────                                                                  │
    │bits              ││ 0000                                                                         │
    │                  ││────────────                                                                  │
    │                  ││────────────                                                                  │
    │code              ││ 0000                                                                         │
    │                  ││────────────                                                                  │
    │                  ││────────────                                                                  │
    │code_length_minus1││ 0                                                                            │
    │                  ││────────────                                                                  │
    │code_write        ││                                                                              │
    │                  ││────────────                                                                  │
    │                  ││────────────                                                                  │
    │destination_identi││ 0                                                                            │
    │                  ││────────────                                                                  │
    │                  ││────────────                                                                  │
    │num_codes_at_lengt││ 00                                                                           │
    │                  ││────────────                                                                  │
    │start             ││                                                                              │
    │                  ││────────────                                                                  │
    │                  ││────────────                                                                  │
    │table_class       ││ 0                                                                            │
    │                  ││────────────                                                                  │
    │done_             ││────────────                                                                  │
    │                  ││                                                                              │
    │                  ││────────────                                                                  │
    │read_bits         ││ 00                                                                           │
    │                  ││────────────                                                                  │
    │                  ││                                                                              │
    │                  ││                                                                              │
    │                  ││                                                                              │
    │                  ││                                                                              │
    │                  ││                                                                              │
    │                  ││                                                                              │
    │                  ││                                                                              │
    └──────────────────┘└──────────────────────────────────────────────────────────────────────────────┘ |}]
;;
