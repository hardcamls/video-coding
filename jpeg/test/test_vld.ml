open! Core
open Hardcaml
open Hardcaml_waveterm
module Vld = Hardcaml_jpeg.Vld
module Sim = Cyclesim.With_interface (Vld.I) (Vld.O)

let%expect_test "" =
  let sim = Sim.create (Vld.create (Scope.create ())) in
  let waves, sim = Waveform.create sim in
  Cyclesim.cycle sim;
  Waveform.print ~display_width:90 ~display_height:40 waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────┐
    │clock             ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───│
    │                  ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   │
    │clear             ││                                                                    │
    │                  ││────────                                                            │
    │                  ││────────                                                            │
    │bits              ││ 0000                                                               │
    │                  ││────────                                                            │
    │bits_valid        ││                                                                    │
    │                  ││────────                                                            │
    │start             ││                                                                    │
    │                  ││────────                                                            │
    │                  ││────────                                                            │
    │coef              ││ 000                                                                │
    │                  ││────────                                                            │
    │error_binary_varia││                                                                    │
    │                  ││────────                                                            │
    │                  ││────────                                                            │
    │read_bits         ││ 00                                                                 │
    │                  ││────────                                                            │
    │                  ││────────                                                            │
    │run               ││ 0                                                                  │
    │                  ││────────                                                            │
    │write             ││                                                                    │
    │                  ││────────                                                            │
    │                  ││                                                                    │
    │                  ││                                                                    │
    │                  ││                                                                    │
    │                  ││                                                                    │
    │                  ││                                                                    │
    │                  ││                                                                    │
    │                  ││                                                                    │
    │                  ││                                                                    │
    │                  ││                                                                    │
    │                  ││                                                                    │
    │                  ││                                                                    │
    │                  ││                                                                    │
    │                  ││                                                                    │
    │                  ││                                                                    │
    └──────────────────┘└────────────────────────────────────────────────────────────────────┘ |}]
;;
