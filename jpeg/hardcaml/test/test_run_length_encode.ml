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
  let cycle () =
    Cyclesim.cycle sim;
    if Bits.to_bool !(outputs.rle_out.valid)
    then
      results
        := { Rle.run = Bits.to_int !(outputs.rle_out.value.run)
           ; coef = Bits.to_int !(outputs.rle_out.value.coef)
           }
           :: !results
  in
  for _ = 0 to Hardcaml_jpeg.Quant.pipeline_depth - 1 do
    cycle ()
  done;
  for i = 0 to 63 do
    inputs.coef := Bits.of_int ~width:12 coefs.(i);
    cycle ()
  done;
  for _ = 0 to 3 do
    cycle ()
  done;
  let results = List.rev !results in
  if verbose then print_s [%message (results : Rle.t)];
  let result = Rle.to_array results in
  if not (Array.equal Int.equal result coefs)
  then raise_s [%message "Mismatch" (result : int array) (coefs : int array)];
  waves
;;

let print ?(wave_width = -1) ?(start_cycle = 0) waves =
  Option.iter
    waves
    ~f:(Waveform.print ~display_width:100 ~display_height:32 ~wave_width ~start_cycle)
;;

let%expect_test "All zeros" =
  test ~waves:true ~verbose:true [ { run = 63; coef = 0 } ] |> print;
  [%expect
    {|
    (results (((run 0) (coef 0)) ((run 62) (coef 0))))
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────┐
    │clock             ││╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥│
    │                  ││╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨│
    │clear             ││─┐                                                                            │
    │                  ││ └────────────────────────────────────────────────────────────────────────    │
    │                  ││──────────────────────────────────────────────────────────────────────────    │
    │coef_in           ││ 000                                                                          │
    │                  ││──────────────────────────────────────────────────────────────────────────    │
    │start             ││ ┌┐                                                                           │
    │                  ││─┘└───────────────────────────────────────────────────────────────────────    │
    │done_             ││──┐                                                                   ┌───    │
    │                  ││  └───────────────────────────────────────────────────────────────────┘       │
    │                  ││───┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬───    │
    │quant_address     ││ 00││││││││││││││││││││││││││││││││││││││││││││││││││││││││││││││││││││04     │
    │                  ││───┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴───    │
    │quant_read        ││  ┌───────────────────────────────────────────────────────────────┐           │
    │                  ││──┘                                                               └───────    │
    │valid             ││       ┌┐                                                             ┌┐      │
    │                  ││───────┘└─────────────────────────────────────────────────────────────┘└──    │
    │                  ││──────────────────────────────────────────────────────────────────────────    │
    │value$coef        ││ 000                                                                          │
    │                  ││──────────────────────────────────────────────────────────────────────────    │
    │value$dc          ││       ┌┐                                                                     │
    │                  ││───────┘└─────────────────────────────────────────────────────────────────    │
    │value$last        ││                                                                      ┌┐      │
    │                  ││──────────────────────────────────────────────────────────────────────┘└──    │
    │                  ││─────────┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬─    │
    │value$run         ││ 00      ││││││││││││││││││││││││││││││││││││││││││││││││││││││││││││││││.    │
    │                  ││─────────┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴─    │
    │                  ││──┬───┬┬──────────────────────────────────────────────────────────────┬───    │
    │STATE             ││ 0│1  ││3                                                             │0      │
    └──────────────────┘└──────────────────────────────────────────────────────────────────────────────┘ |}]
;;

let%expect_test "Single dc coef" =
  test ~waves:false ~verbose:true [ { run = 0; coef = 1 }; { run = 62; coef = 0 } ]
  |> print;
  [%expect {| (results (((run 0) (coef 1)) ((run 62) (coef 0)))) |}]
;;

let%expect_test "first and last" =
  test ~waves:false ~verbose:true [ { run = 0; coef = 1 }; { run = 62; coef = 2 } ]
  |> print ~start_cycle:60 ~wave_width:1;
  [%expect {|
    (results (((run 0) (coef 1)) ((run 62) (coef 2)))) |}]
;;

let%expect_test "first and second" =
  test
    ~waves:false
    ~verbose:true
    [ { run = 0; coef = 1 }; { run = 0; coef = 2 }; { run = 61; coef = 0 } ]
  |> print;
  [%expect {| (results (((run 0) (coef 1)) ((run 0) (coef 2)) ((run 61) (coef 0)))) |}]
;;

let%expect_test "first, second and last" =
  test
    ~waves:false
    ~verbose:true
    [ { run = 0; coef = 1 }; { run = 0; coef = 2 }; { run = 61; coef = 3 } ]
  |> print;
  [%expect {| (results (((run 0) (coef 1)) ((run 0) (coef 2)) ((run 61) (coef 3)))) |}]
;;

let%expect_test "first and last two" =
  test
    ~waves:false
    ~verbose:true
    [ { run = 0; coef = 1 }; { run = 61; coef = 2 }; { run = 0; coef = 3 } ]
  |> print;
  [%expect {| (results (((run 0) (coef 1)) ((run 61) (coef 2)) ((run 0) (coef 3)))) |}]
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
