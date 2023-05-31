open! Core
open! Hardcaml
module Encoder = Hardcaml_jpeg.Encoder_datapath
module Sim = Cyclesim.With_interface (Encoder.I) (Encoder.O)

let%expect_test "" =
  let _ = Sim.create (Encoder.create (Scope.create ~flatten_design:true ())) in
  ()
;;
