open! Core
open Hardcaml
open Hardcaml_waveterm
module Vld = Hardcaml_jpeg.Vld.With_reader
module Sim = Cyclesim.With_interface (Vld.I) (Vld.O)

let%expect_test "" =
  let sim =
    Sim.create
      ~config:Cyclesim.Config.trace_all
      (Vld.create (Scope.create ~auto_label_hierarchical_ports:true ()))
  in
  let waves, sim = Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  inputs.clocking.clear := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clocking.clear := Bits.gnd;
  inputs.start := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.start := Bits.gnd;
  inputs.bits_in_available := Bits.vdd;
  for _ = 0 to 10 do
    inputs.bits_in := Bits.of_int ~width:16 (-1);
    Cyclesim.cycle sim
  done;
  Cyclesim.cycle sim;
  Waveform.print ~display_width:90 ~display_height:40 waves;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  ("Topological_sort.sort encountered cycle"
    (instantiation wire select wire instantiation wire select wire))
  Raised at Base__Error.raise in file "src/error.ml" (inlined), line 9, characters 14-30
  Called from Base__Or_error.ok_exn in file "src/or_error.ml", line 92, characters 17-32
  Called from Hardcaml__Cyclesim_schedule.create in file "hardcaml/src/cyclesim_schedule.ml", line 87, characters 9-73
  Called from Hardcaml__Cyclesim_compile.create in file "hardcaml/src/cyclesim_compile.ml", line 753, characters 15-53
  Called from Hardcaml__Cyclesim.With_interface.create in file "hardcaml/src/cyclesim.ml", line 118, characters 14-36
  Called from Hardcaml_jpeg_test__Test_vld.(fun) in file "video-coding/jpeg/test/test_vld.ml", line 9, characters 4-127
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19 |}]
;;
