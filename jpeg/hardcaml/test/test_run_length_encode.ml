open! Core
open! Hardcaml
open Hardcaml_waveterm
module Huffman = Hardcaml_jpeg.Run_length_encode
module Sim = Cyclesim.With_interface (Huffman.I) (Huffman.O)

module Rle = struct
  type run_length_code =
    { run : int
    ; coef : int
    }
  [@@deriving sexp_of]

  type t = run_length_code list [@@deriving sexp_of]

  let to_array t =
    let a = Array.init 64 ~f:(fun _ -> 0) in
    let pos = ref 0 in
    List.iter t ~f:(fun { run; coef } ->
        pos := !pos + run;
        a.(!pos) <- coef;
        pos := !pos + 1);
    assert (!pos = 64);
    a
  ;;
end

let test ?(waves = false) ?(verbose = false) (rle : Rle.t) =
  let sim =
    Sim.create
      ~config:Cyclesim.Config.trace_all
      (Huffman.create (Scope.create ~flatten_design:true ()))
  in
  let waves, sim =
    if waves
    then (
      let waves, sim = Waveform.create sim in
      Some waves, sim)
    else None, sim
  in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  inputs.clocking.clear := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clocking.clear := Bits.gnd;
  inputs.start := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.start := Bits.gnd;
  let coefs = Rle.to_array rle in
  let results = ref [] in
  let address = Array.init Hardcaml_jpeg.Quant.pipeline_depth ~f:(Fn.const 0) in
  let shift_address () =
    for i = 1 to Array.length address - 1 do
      address.(i - 1) <- address.(i)
    done
  in
  let cycle () =
    Cyclesim.cycle sim;
    if Bits.to_bool !(outputs.rle_out.valid)
    then
      results
        := { Rle.run = Bits.to_int !(outputs.rle_out.value.run)
           ; coef = Bits.to_int !(outputs.rle_out.value.coef)
           }
           :: !results;
    inputs.coef := Bits.of_int ~width:12 coefs.(address.(0));
    shift_address ();
    if Bits.to_bool !(outputs.quant_read)
    then address.(Array.length address - 1) <- Bits.to_int !(outputs.quant_address)
  in
  while not (Bits.to_bool !(outputs.done_)) do
    cycle ()
  done;
  for _ = 0 to 3 do
    cycle ()
  done;
  let results = List.rev !results in
  if verbose then print_s [%message (results : Rle.t)];
  let result = Rle.to_array results in
  if not (Array.equal Int.equal result coefs)
  then (
    let mismatch = [%message "Mismatch" (result : int array) (coefs : int array)] in
    if Option.is_some waves then print_s mismatch else raise_s mismatch);
  waves
;;

let print ?(wave_width = -1) ?(start_cycle = 0) waves =
  Option.iter
    waves
    ~f:(Waveform.print ~display_width:100 ~display_height:32 ~wave_width ~start_cycle)
;;

let%expect_test "All zeros" =
  test ~waves:true ~verbose:true [ { run = 63; coef = 0 } ] |> print ~wave_width:2;
  [%expect
    {|
    (results (((run 0) (coef 0)) ((run 62) (coef 0))))
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────┐
    │clock             ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  │
    │                  ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──│
    │clear             ││──────┐                                                                       │
    │                  ││      └───────────────────────────────────────────────────────────────────────│
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │coef_in           ││ 000                                                                          │
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │start             ││      ┌─────┐                                                                 │
    │                  ││──────┘     └─────────────────────────────────────────────────────────────────│
    │done_             ││────────────┐                                                                 │
    │                  ││            └─────────────────────────────────────────────────────────────────│
    │                  ││──────────────────┬─────┬─────┬─────┬─────┬───────────┬─────┬─────┬─────┬─────│
    │quant_address     ││ 00               │01   │02   │03   │04   │05         │06   │07   │08   │09   │
    │                  ││──────────────────┴─────┴─────┴─────┴─────┴───────────┴─────┴─────┴─────┴─────│
    │quant_read        ││            ┌─────────────────────────────┐     ┌─────────────────────────────│
    │                  ││────────────┘                             └─────┘                             │
    │valid             ││                                          ┌─────┐                             │
    │                  ││──────────────────────────────────────────┘     └─────────────────────────────│
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │value$coef        ││ 000                                                                          │
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │value$dc          ││                                          ┌───────────────────────────────────│
    │                  ││──────────────────────────────────────────┘                                   │
    │value$last        ││                                                                              │
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │value$run         ││ 00                                                                           │
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │                  ││────────────┬───────────────────────┬─────┬─────┬─────────────────────────────│
    │STATE             ││ 0          │1                      │2    │3    │4                            │
    └──────────────────┘└──────────────────────────────────────────────────────────────────────────────┘ |}]
;;

let%expect_test "Single dc coef" =
  test ~waves:true ~verbose:true [ { run = 0; coef = 1 }; { run = 62; coef = 0 } ]
  |> print ~wave_width:2;
  [%expect
    {|
    (results (((run 0) (coef 1)) ((run 62) (coef 0))))
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────┐
    │clock             ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  │
    │                  ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──│
    │clear             ││──────┐                                                                       │
    │                  ││      └───────────────────────────────────────────────────────────────────────│
    │                  ││──────────────────┬───────────────────────┬───────────────────────────────────│
    │coef_in           ││ 000              │001                    │000                                │
    │                  ││──────────────────┴───────────────────────┴───────────────────────────────────│
    │start             ││      ┌─────┐                                                                 │
    │                  ││──────┘     └─────────────────────────────────────────────────────────────────│
    │done_             ││────────────┐                                                                 │
    │                  ││            └─────────────────────────────────────────────────────────────────│
    │                  ││──────────────────┬─────┬─────┬─────┬─────┬───────────┬─────┬─────┬─────┬─────│
    │quant_address     ││ 00               │01   │02   │03   │04   │05         │06   │07   │08   │09   │
    │                  ││──────────────────┴─────┴─────┴─────┴─────┴───────────┴─────┴─────┴─────┴─────│
    │quant_read        ││            ┌─────────────────────────────┐     ┌─────────────────────────────│
    │                  ││────────────┘                             └─────┘                             │
    │valid             ││                                          ┌─────┐                             │
    │                  ││──────────────────────────────────────────┘     └─────────────────────────────│
    │                  ││──────────────────────────────────────────┬───────────────────────────────────│
    │value$coef        ││ 000                                      │001                                │
    │                  ││──────────────────────────────────────────┴───────────────────────────────────│
    │value$dc          ││                                          ┌───────────────────────────────────│
    │                  ││──────────────────────────────────────────┘                                   │
    │value$last        ││                                                                              │
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │value$run         ││ 00                                                                           │
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │                  ││────────────┬───────────────────────┬─────┬─────┬─────────────────────────────│
    │STATE             ││ 0          │1                      │2    │3    │4                            │
    └──────────────────┘└──────────────────────────────────────────────────────────────────────────────┘ |}]
;;

let%expect_test "first and last" =
  test ~waves:true ~verbose:true [ { run = 0; coef = 1 }; { run = 62; coef = 2 } ]
  |> print ~start_cycle:60 ~wave_width:1;
  [%expect
    {|
    (results (((run 0) (coef 1)) ((run 62) (coef 2))))
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────┐
    │clock             ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─│
    │                  ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ │
    │clear             ││                                                                              │
    │                  ││────────────────────────────────────────────────────────────                  │
    │                  ││────────────────────────────────────────┬───────────────────                  │
    │coef_in           ││ 000                                    │002                                  │
    │                  ││────────────────────────────────────────┴───────────────────                  │
    │start             ││                                                                              │
    │                  ││────────────────────────────────────────────────────────────                  │
    │done_             ││                                            ┌───────────────                  │
    │                  ││────────────────────────────────────────────┘                                 │
    │                  ││────┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───────────────                  │
    │quant_address     ││ 39 │3A │3B │3C │3D │3E │3F │00 │01 │02 │03 │04                               │
    │                  ││────┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───────────────                  │
    │quant_read        ││────────────────────────────┐                                                 │
    │                  ││                            └───────────────────────────────                  │
    │valid             ││                                            ┌───┐                             │
    │                  ││────────────────────────────────────────────┘   └───────────                  │
    │                  ││────────────────────────────────────────────┬───────────────                  │
    │value$coef        ││ 001                                        │002                              │
    │                  ││────────────────────────────────────────────┴───────────────                  │
    │value$dc          ││────────────────────────────────────────────┐                                 │
    │                  ││                                            └───────────────                  │
    │value$last        ││                                            ┌───────────────                  │
    │                  ││────────────────────────────────────────────┘                                 │
    │                  ││────────────────────────────────────────────┬───────────────                  │
    │value$run         ││ 00                                         │3E                               │
    │                  ││────────────────────────────────────────────┴───────────────                  │
    │                  ││────────────────────────────────────────────┬───────────────                  │
    │STATE             ││ 4                                          │0                                │
    └──────────────────┘└──────────────────────────────────────────────────────────────────────────────┘ |}]
;;

let%expect_test "first and second" =
  test
    ~waves:true
    ~verbose:true
    [ { run = 0; coef = 1 }; { run = 0; coef = 2 }; { run = 61; coef = 0 } ]
  |> print ~wave_width:2;
  [%expect
    {|
    (results (((run 0) (coef 1)) ((run 62) (coef 0))))
    (Mismatch
     (result
      (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
     (coefs
      (1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────┐
    │clock             ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  │
    │                  ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──│
    │clear             ││──────┐                                                                       │
    │                  ││      └───────────────────────────────────────────────────────────────────────│
    │                  ││──────────────────┬───────────────────────┬─────┬─────────────────────────────│
    │coef_in           ││ 000              │001                    │002  │000                          │
    │                  ││──────────────────┴───────────────────────┴─────┴─────────────────────────────│
    │start             ││      ┌─────┐                                                                 │
    │                  ││──────┘     └─────────────────────────────────────────────────────────────────│
    │done_             ││────────────┐                                                                 │
    │                  ││            └─────────────────────────────────────────────────────────────────│
    │                  ││──────────────────┬─────┬─────┬─────┬─────┬───────────┬─────┬─────┬─────┬─────│
    │quant_address     ││ 00               │01   │02   │03   │04   │05         │06   │07   │08   │09   │
    │                  ││──────────────────┴─────┴─────┴─────┴─────┴───────────┴─────┴─────┴─────┴─────│
    │quant_read        ││            ┌─────────────────────────────┐     ┌─────────────────────────────│
    │                  ││────────────┘                             └─────┘                             │
    │valid             ││                                          ┌─────┐                             │
    │                  ││──────────────────────────────────────────┘     └─────────────────────────────│
    │                  ││──────────────────────────────────────────┬───────────────────────────────────│
    │value$coef        ││ 000                                      │001                                │
    │                  ││──────────────────────────────────────────┴───────────────────────────────────│
    │value$dc          ││                                          ┌───────────────────────────────────│
    │                  ││──────────────────────────────────────────┘                                   │
    │value$last        ││                                                                              │
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │value$run         ││ 00                                                                           │
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │                  ││────────────┬───────────────────────┬─────┬─────┬─────────────────────────────│
    │STATE             ││ 0          │1                      │2    │3    │4                            │
    └──────────────────┘└──────────────────────────────────────────────────────────────────────────────┘ |}]
;;

let%expect_test "first, second and last" =
  test
    ~waves:false
    ~verbose:true
    [ { run = 0; coef = 1 }; { run = 0; coef = 2 }; { run = 61; coef = 3 } ]
  |> print;
  [%expect.unreachable]
  [@@expect.uncaught_exn
    {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Mismatch
    (result
      (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3))
    (coefs
      (1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3)))
  Raised at Base__Error.raise in file "src/error.ml" (inlined), line 9, characters 14-30
  Called from Base__Error.raise_s in file "src/error.ml", line 10, characters 19-40
  Called from Hardcaml_jpeg_test__Test_run_length_encode.test in file "video-coding/jpeg/hardcaml/test/test_run_length_encode.ml", line 83, characters 55-71
  Called from Hardcaml_jpeg_test__Test_run_length_encode.(fun) in file "video-coding/jpeg/hardcaml/test/test_run_length_encode.ml", line 263, characters 2-118
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  (results (((run 0) (coef 1)) ((run 62) (coef 3)))) |}]
;;

let%expect_test "first and last two" =
  test
    ~waves:false
    ~verbose:true
    [ { run = 0; coef = 1 }; { run = 61; coef = 2 }; { run = 0; coef = 3 } ]
  |> print;
  [%expect
    {|
    (results (((run 0) (coef 1)) ((run 61) (coef 2)) ((run 0) (coef 3)))) |}]
;;

let%expect_test "randomish..." =
  test
    ~waves:false
    ~verbose:true
    [ { run = 0; coef = 1 }
    ; { run = 10; coef = 2 }
    ; { run = 0; coef = 3 }
    ; { run = 20; coef = 4 }
    ; { run = 0; coef = 5 }
    ; { run = 0; coef = 6 }
    ; { run = 0; coef = 7 }
    ; { run = 0; coef = 8 }
    ; { run = 0; coef = 9 }
    ; { run = 24; coef = 10 }
    ]
  |> print;
  [%expect
    {|
    (results
     (((run 0) (coef 1)) ((run 10) (coef 2)) ((run 0) (coef 3))
      ((run 20) (coef 4)) ((run 0) (coef 5)) ((run 0) (coef 6))
      ((run 0) (coef 7)) ((run 0) (coef 8)) ((run 0) (coef 9))
      ((run 24) (coef 10)))) |}]
;;
