open Core
open Hardcaml
open Hardcaml_waveterm
module Huffman = Hardcaml_jpeg.Huffman_encode

let size = Hardcaml_jpeg_model.Encoder.size
let hw_size = Huffman.size (module Bits)

let%expect_test "derive size" =
  let test i =
    let bits = Bits.of_int ~width:12 i in
    let size = size i in
    let hw_size = hw_size bits |> Bits.to_int in
    if size <> hw_size
    then raise_s [%message (i : int) (bits : Bits.t) (size : int) (hw_size : int)]
  in
  for i = -2048 to 2047 do
    test i
  done;
  [%expect {| |}]
;;

let mag = Hardcaml_jpeg_model.Encoder.magnitude
let hw_mag = Huffman.mag (module Bits)

let%expect_test "derive magnitude" =
  let test i =
    let bits = Bits.of_int ~width:12 i in
    let size = size i in
    let mag = mag ~size i in
    let hw_mag = hw_mag (Bits.of_int ~width:4 size) bits |> Bits.to_int in
    if mag <> hw_mag
    then
      raise_s [%message (i : int) (bits : Bits.t) (size : int) (mag : int) (hw_mag : int)]
  in
  for i = -2048 to 2047 do
    test i
  done;
  [%expect {| |}]
;;

module Sim = Cyclesim.With_interface (Huffman.I) (Huffman.O)

type run_coef =
  { run : int
  ; coef : int
  }

let data = [| { run = 0; coef = 3 }; { run = 62; coef = 32 } |]
let ( <--. ) a b = a := Bits.of_int ~width:(Bits.width !a) b

let test ?(waves = false) () =
  let sim =
    Sim.create ~config:Cyclesim.Config.trace_all (Huffman.create (Scope.create ()))
  in
  let waves, sim =
    if waves
    then (
      let waves, sim = Waveform.create sim in
      Some waves, sim)
    else None, sim
  in
  let inputs = Cyclesim.inputs sim in
  let _outputs = Cyclesim.outputs sim in
  inputs.clocking.clear := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clocking.clear := Bits.gnd;
  inputs.start := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.start := Bits.gnd;
  let cycle () = Cyclesim.cycle sim in
  let pos = ref 0 in
  for _ = 0 to 10 do
    (inputs.rle_in.value.run
    <--.
    try data.(!pos).run with
    | _ -> 0);
    (inputs.rle_in.value.coef
    <--.
    try data.(!pos).coef with
    | _ -> 0);
    cycle ();
    Int.incr pos
  done;
  waves
;;

let%expect_test "" =
  Option.iter
    (test ~waves:true ())
    ~f:(Waveform.print ~display_width:100 ~display_height:40 ~wave_width:2 ~start_cycle:0);
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  ("[mux] got inputs of different widths"
    ((and (width 12) (arguments (mux mux)))
      (mux (width 16) (select wire) (data (mux mux)))))
  Raised at Base__Error.raise in file "src/error.ml" (inlined), line 9, characters 14-30
  Called from Base__Error.raise_s in file "src/error.ml", line 10, characters 19-40
  Called from Stdlib__List.iter in file "list.ml", line 110, characters 12-15
  Called from Hardcaml__Comb.Make.mux in file "hardcaml/src/comb.ml", line 447, characters 4-30
  Called from Hardcaml_jpeg__Huffman_encode.create in file "video-coding/jpeg/hardcaml/src/huffman_encode.ml", line 112, characters 36-68
  Called from Hardcaml__Circuit.With_interface.create_exn in file "hardcaml/src/circuit.ml", line 398, characters 18-30
  Called from Hardcaml__Cyclesim.With_interface.create in file "hardcaml/src/cyclesim.ml", line 117, characters 18-81
  Called from Hardcaml_jpeg_test__Test_huffman_encode.test in file "video-coding/jpeg/hardcaml/test/test_huffman_encode.ml", line 54, characters 4-83
  Called from Hardcaml_jpeg_test__Test_huffman_encode.(fun) in file "video-coding/jpeg/hardcaml/test/test_huffman_encode.ml", line 90, characters 4-25
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19 |}]
;;
