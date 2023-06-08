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

let%expect_test "simulation" =
  Option.iter
    (test ~waves:true ())
    ~f:(Waveform.print ~display_width:100 ~display_height:25 ~wave_width:2 ~start_cycle:0);
  [%expect{|
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────┐
    │clock             ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  │
    │                  ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──│
    │clear             ││──────┐                                                                       │
    │                  ││      └─────────────────────────────────────────────────────────────────      │
    │luma              ││                                                                              │
    │                  ││────────────────────────────────────────────────────────────────────────      │
    │valid             ││                                                                              │
    │                  ││────────────────────────────────────────────────────────────────────────      │
    │                  ││──────┬─────┬─────┬─────────────────────────────────────────────────────      │
    │value$coef        ││ 000  │003  │020  │000                                                        │
    │                  ││──────┴─────┴─────┴─────────────────────────────────────────────────────      │
    │value$dc          ││                                                                              │
    │                  ││────────────────────────────────────────────────────────────────────────      │
    │                  ││────────────┬─────┬─────────────────────────────────────────────────────      │
    │value$run         ││ 00         │3E   │00                                                         │
    │                  ││────────────┴─────┴─────────────────────────────────────────────────────      │
    │                  ││──────┬─────┬─────┬─────────────────────────────────────────────────────      │
    │bits              ││ 0000 │0004 │FFFA │0000                                                       │
    │                  ││──────┴─────┴─────┴─────────────────────────────────────────────────────      │
    │                  ││────────────────────────────────────────────────────────────────────────      │
    │num_bits          ││ 00                                                                           │
    │                  ││────────────────────────────────────────────────────────────────────────      │
    │STATE             ││                                                                              │
    └──────────────────┘└──────────────────────────────────────────────────────────────────────────────┘ |}]
;;
