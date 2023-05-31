open! Core
open Hardcaml
open Hardcaml_waveterm
module Encoder = Hardcaml_jpeg.Encoder_datapath
module Sim = Cyclesim.With_interface (Encoder.I) (Encoder.O)

let test ?(waves = false) () =
  let sim = Sim.create (Encoder.create (Scope.create ~flatten_design:true ())) in
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

let%expect_test "instantiate" = ignore (test ~waves:false () : Waveform.t option)
