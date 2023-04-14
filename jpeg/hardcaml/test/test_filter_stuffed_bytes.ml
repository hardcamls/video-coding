open Core
open Hardcaml
open Hardcaml_waveterm
module Fsb = Hardcaml_jpeg.Filter_stuffed_bytes
module Sim = Cyclesim.With_interface (Fsb.I) (Fsb.O)

let step_test steps =
  let sim = Sim.create ~config:Cyclesim.Config.trace_all (Fsb.create (Scope.create ())) in
  let waves, sim = Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  inputs.clocking.clear := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clocking.clear := Bits.gnd;
  let step valid data ready =
    inputs.i_valid := Bits.of_bool valid;
    inputs.i_data := Bits.of_int ~width:16 data;
    inputs.o_ready := Bits.of_bool ready;
    Cyclesim.cycle sim
  in
  List.iter steps ~f:(fun (valid, data, ready) -> step valid data ready);
  for _ = 0 to 3 do
    step false 0 false
  done;
  Waveform.print ~wave_width:2 ~display_width:110 ~display_height:46 waves
;;

let%expect_test "ready/valid" =
  step_test
    [ true, 0x12, false
    ; true, 0x34, true
    ; false, 0, true
    ; false, 0x45, false
    ; false, 0x45, true
    ; true, 0x45, true
    ; false, 0x0, true
    ];
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────────────────────────┐
    │clock             ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐│
    │                  ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └│
    │clear             ││──────┐                                                                                 │
    │                  ││      └─────────────────────────────────────────────────────────────────                │
    │                  ││──────┬─────┬─────┬─────┬─────────────────┬─────────────────────────────                │
    │i_data            ││ 0000 │0012 │0034 │0000 │0045             │0000                                         │
    │                  ││──────┴─────┴─────┴─────┴─────────────────┴─────────────────────────────                │
    │i_valid           ││      ┌───────────┐                 ┌─────┐                                             │
    │                  ││──────┘           └─────────────────┘     └─────────────────────────────                │
    │o_ready           ││            ┌───────────┐     ┌─────────────────┐                                       │
    │                  ││────────────┘           └─────┘                 └───────────────────────                │
    │i_ready           ││      ┌───────────┐                 ┌─────┐                                             │
    │                  ││──────┘           └─────────────────┘     └─────────────────────────────                │
    │                  ││────────────┬─────┬───────────────────────┬─────────────────────────────                │
    │o_data            ││ 0000       │0012 │0034                   │0045                                         │
    │                  ││────────────┴─────┴───────────────────────┴─────────────────────────────                │
    │o_valid           ││            ┌───────────┐                 ┌─────┐                                       │
    │                  ││────────────┘           └─────────────────┘     └───────────────────────                │
    │                  ││────────────┬─────┬───────────────────────┬─────────────────────────────                │
    │buffered_byte     ││ 00         │12   │34                     │45                                           │
    │                  ││────────────┴─────┴───────────────────────┴─────────────────────────────                │
    │                  ││────────────────────────────────────────────────────────────────────────                │
    │data_1            ││ 00                                                                                     │
    │                  ││────────────────────────────────────────────────────────────────────────                │
    │                  ││──────┬─────┬─────┬─────┬─────────────────┬─────────────────────────────                │
    │data_2            ││ 00   │12   │34   │00   │45               │00                                           │
    │                  ││──────┴─────┴─────┴─────┴─────────────────┴─────────────────────────────                │
    │gnd               ││                                                                                        │
    │                  ││────────────────────────────────────────────────────────────────────────                │
    │have_buffered_byte││                                                                                        │
    │                  ││────────────────────────────────────────────────────────────────────────                │
    │is_00_1           ││────────────────────────────────────────────────────────────────────────                │
    │                  ││                                                                                        │
    │is_00_2           ││──────┐           ┌─────┐                 ┌─────────────────────────────                │
    │                  ││      └───────────┘     └─────────────────┘                                             │
    │is_ff_1           ││                                                                                        │
    │                  ││────────────────────────────────────────────────────────────────────────                │
    │last_byte_was_ff  ││                                                                                        │
    │                  ││────────────────────────────────────────────────────────────────────────                │
    │remove_first_byte ││                                                                                        │
    │                  ││────────────────────────────────────────────────────────────────────────                │
    │remove_second_byte││                                                                                        │
    │                  ││────────────────────────────────────────────────────────────────────────                │
    │vdd               ││────────────────────────────────────────────────────────────────────────                │
    └──────────────────┘└────────────────────────────────────────────────────────────────────────────────────────┘ |}]
;;

let%expect_test "test byte stuffing removal" =
  step_test
    [ true, 0xaabb, true
    ; true, 0xff00, true
    ; true, 0xccdd, true
    ; true, 0xeeff, true
    ; false, 0x0, true
    ];
  [%expect
    {|
   ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────────────────────────┐
   │clock             ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐│
   │                  ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └│
   │clear             ││──────┐                                                                                 │
   │                  ││      └─────────────────────────────────────────────────────                            │
   │                  ││──────┬─────┬─────┬─────┬─────┬─────────────────────────────                            │
   │i_data            ││ 0000 │AABB │FF00 │CCDD │EEFF │0000                                                     │
   │                  ││──────┴─────┴─────┴─────┴─────┴─────────────────────────────                            │
   │i_valid           ││      ┌───────────────────────┐                                                         │
   │                  ││──────┘                       └─────────────────────────────                            │
   │o_ready           ││      ┌─────────────────────────────┐                                                   │
   │                  ││──────┘                             └───────────────────────                            │
   │i_ready           ││      ┌───────────────────────┐                                                         │
   │                  ││──────┘                       └─────────────────────────────                            │
   │                  ││────────────┬───────────┬─────┬─────────────────────────────                            │
   │o_data            ││ 0000       │AABB       │FFCC │DDEE                                                     │
   │                  ││────────────┴───────────┴─────┴─────────────────────────────                            │
   │o_valid           ││            ┌─────┐     ┌───────────┐                                                   │
   │                  ││────────────┘     └─────┘           └───────────────────────                            │
   │                  ││────────────┬─────┬─────┬─────┬─────────────────────────────                            │
   │buffered_byte     ││ 00         │BB   │FF   │DD   │FF                                                       │
   │                  ││────────────┴─────┴─────┴─────┴─────────────────────────────                            │
   │                  ││──────┬─────┬─────┬─────┬─────┬─────────────────────────────                            │
   │data_1            ││ 00   │AA   │FF   │CC   │EE   │00                                                       │
   │                  ││──────┴─────┴─────┴─────┴─────┴─────────────────────────────                            │
   │                  ││──────┬─────┬─────┬─────┬─────┬─────────────────────────────                            │
   │data_2            ││ 00   │BB   │00   │DD   │FF   │00                                                       │
   │                  ││──────┴─────┴─────┴─────┴─────┴─────────────────────────────                            │
   │gnd               ││                                                                                        │
   │                  ││────────────────────────────────────────────────────────────                            │
   │have_buffered_byte││                  ┌─────────────────────────────────────────                            │
   │                  ││──────────────────┘                                                                     │
   │is_00_1           ││──────┐                       ┌─────────────────────────────                            │
   │                  ││      └───────────────────────┘                                                         │
   │is_00_2           ││──────┐     ┌─────┐           ┌─────────────────────────────                            │
   │                  ││      └─────┘     └───────────┘                                                         │
   │is_ff_1           ││            ┌─────┐                                                                     │
   │                  ││────────────┘     └─────────────────────────────────────────                            │
   │last_byte_was_ff  ││                  ┌─────┐     ┌─────────────────────────────                            │
   │                  ││──────────────────┘     └─────┘                                                         │
   │remove_first_byte ││                              ┌─────────────────────────────                            │
   │                  ││──────────────────────────────┘                                                         │
   │remove_second_byte││            ┌─────┐                                                                     │
   │                  ││────────────┘     └─────────────────────────────────────────                            │
   │vdd               ││────────────────────────────────────────────────────────────                            │
   └──────────────────┘└────────────────────────────────────────────────────────────────────────────────────────┘ |}]
;;

let%expect_test "byte stuffing crossing words" =
  step_test
    [ true, 0xaabb, true
    ; true, 0xccff, true
    ; true, 0x00dd, true
    ; true, 0xeeff, true
    ; false, 0x0, true
    ];
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────────────────────────┐
    │clock             ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐│
    │                  ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └│
    │clear             ││──────┐                                                                                 │
    │                  ││      └─────────────────────────────────────────────────────                            │
    │                  ││──────┬─────┬─────┬─────┬─────┬─────────────────────────────                            │
    │i_data            ││ 0000 │AABB │CCFF │00DD │EEFF │0000                                                     │
    │                  ││──────┴─────┴─────┴─────┴─────┴─────────────────────────────                            │
    │i_valid           ││      ┌───────────────────────┐                                                         │
    │                  ││──────┘                       └─────────────────────────────                            │
    │o_ready           ││      ┌─────────────────────────────┐                                                   │
    │                  ││──────┘                             └───────────────────────                            │
    │i_ready           ││      ┌───────────────────────┐                                                         │
    │                  ││──────┘                       └─────────────────────────────                            │
    │                  ││────────────┬─────┬───────────┬─────────────────────────────                            │
    │o_data            ││ 0000       │AABB │CCFF       │DDEE                                                     │
    │                  ││────────────┴─────┴───────────┴─────────────────────────────                            │
    │o_valid           ││            ┌───────────┐     ┌─────┐                                                   │
    │                  ││────────────┘           └─────┘     └───────────────────────                            │
    │                  ││────────────┬─────┬─────┬─────┬─────────────────────────────                            │
    │buffered_byte     ││ 00         │BB   │FF   │DD   │FF                                                       │
    │                  ││────────────┴─────┴─────┴─────┴─────────────────────────────                            │
    │                  ││──────┬─────┬─────┬─────┬─────┬─────────────────────────────                            │
    │data_1            ││ 00   │AA   │CC   │00   │EE   │00                                                       │
    │                  ││──────┴─────┴─────┴─────┴─────┴─────────────────────────────                            │
    │                  ││──────┬─────┬─────┬─────┬─────┬─────────────────────────────                            │
    │data_2            ││ 00   │BB   │FF   │DD   │FF   │00                                                       │
    │                  ││──────┴─────┴─────┴─────┴─────┴─────────────────────────────                            │
    │gnd               ││                                                                                        │
    │                  ││────────────────────────────────────────────────────────────                            │
    │have_buffered_byte││                        ┌───────────────────────────────────                            │
    │                  ││────────────────────────┘                                                               │
    │is_00_1           ││──────┐           ┌─────┐     ┌─────────────────────────────                            │
    │                  ││      └───────────┘     └─────┘                                                         │
    │is_00_2           ││──────┐                       ┌─────────────────────────────                            │
    │                  ││      └───────────────────────┘                                                         │
    │is_ff_1           ││                                                                                        │
    │                  ││────────────────────────────────────────────────────────────                            │
    │last_byte_was_ff  ││                  ┌─────┐     ┌─────────────────────────────                            │
    │                  ││──────────────────┘     └─────┘                                                         │
    │remove_first_byte ││                  ┌─────┐     ┌─────────────────────────────                            │
    │                  ││──────────────────┘     └─────┘                                                         │
    │remove_second_byte││                                                                                        │
    │                  ││────────────────────────────────────────────────────────────                            │
    │vdd               ││────────────────────────────────────────────────────────────                            │
    └──────────────────┘└────────────────────────────────────────────────────────────────────────────────────────┘ |}]
;;

let add_stuffing d =
  let size = Array.length d in
  (* count ffs to make space in the output *)
  let num_ffs =
    Array.fold d ~init:0 ~f:(fun acc x -> if x = 0xff then acc + 1 else acc)
  in
  (* Add 0 byte stuffing *)
  let pos = ref 0 in
  let q = Array.init (size + num_ffs) ~f:(Fn.const 0) in
  for i = 0 to size - 1 do
    q.(!pos) <- d.(i);
    Int.incr pos;
    if d.(i) = 0xff
    then (
      q.(!pos) <- 0;
      Int.incr pos)
  done;
  q
;;

let create_entropy_bits ~min ~size =
  (* create some random data between [min..255].  THe larger [min is, the 
       more ff's we'll end up with which is better for testing, . ] *)
  let d = Array.init size ~f:(fun _ -> Random.int (256 - min) + min) in
  d, add_stuffing d
;;

let test ?(waves = false) ?(verbose = false) d q =
  if verbose then print_s [%message (d : Int.Hex.t array) (q : Int.Hex.t array)];
  let sim = Sim.create ~config:Cyclesim.Config.trace_all (Fsb.create (Scope.create ())) in
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
  let pos = ref 0 in
  inputs.i_valid := Bits.vdd;
  inputs.o_ready := Bits.vdd;
  let r = ref [] in
  let cycle () =
    Cyclesim.cycle sim;
    if Bits.to_bool !(outputs.o_valid) && Bits.to_bool !(inputs.o_ready)
    then
      r
        := Bits.(to_int !(outputs.o_data).:[7, 0])
           :: Bits.(to_int !(outputs.o_data).:[15, 8])
           :: !r
  in
  while !pos < Array.length q do
    let getq i =
      try q.(!pos + i) with
      | _ -> 0x0
    in
    inputs.i_data
      := Bits.concat_msb (List.map [ getq 0; getq 1 ] ~f:(Bits.of_int ~width:8));
    pos := !pos + 2;
    cycle ();
    if Random.int 5 = 0
    then (
      inputs.o_ready := Bits.gnd;
      Cyclesim.cycle sim;
      inputs.o_ready := Bits.vdd)
  done;
  inputs.i_valid := Bits.gnd;
  for _ = 0 to 3 do
    cycle ()
  done;
  let r = List.rev !r |> Array.of_list in
  if verbose then print_s [%message (r : Int.Hex.t array)];
  (* Because we dont currently flush properly, the last byte might not be output. *)
  let length_r = Array.length r in
  let length_d = Array.length d in
  let error msg = if Option.is_some waves then print_s msg else raise_s msg in
  if length_r <> length_d && length_r + 1 <> length_d
  then error [%message "got invalid output length" (length_d : int) (length_r : int)];
  for i = 0 to Int.min length_r length_d - 1 do
    if d.(i) <> r.(i)
    then
      error [%message "byte mismatch" (i : int) (d.(i) : Int.Hex.t) (r.(i) : Int.Hex.t)]
  done;
  waves
;;

let%expect_test "consequetive stuffing" =
  let d = [| 0xff; 0xff; 0xa; 0xb; 0xff; 0xc; 0xff; 0xff; 0xd |] in
  let q = add_stuffing d in
  let waves = test ~verbose:true ~waves:true d q in
  Option.iter
    waves
    ~f:(Waveform.print ~wave_width:2 ~display_width:110 ~display_height:46);
  [%expect
    {|
    ((d (0xff 0xff 0xa 0xb 0xff 0xc 0xff 0xff 0xd))
     (q (0xff 0x0 0xff 0x0 0xa 0xb 0xff 0x0 0xc 0xff 0x0 0xff 0x0 0xd)))
    (r (0xff 0xff 0xa 0xb 0xc 0xff 0xff 0xd))
    ("byte mismatch" (i 4) ("d.(i)" 0xff) ("r.(i)" 0xc))
    ("byte mismatch" (i 5) ("d.(i)" 0xc) ("r.(i)" 0xff))
    ("byte mismatch" (i 7) ("d.(i)" 0xff) ("r.(i)" 0xd))
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────────────────────────┐
    │clock             ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐│
    │                  ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └│
    │clear             ││──────┐                                                                                 │
    │                  ││      └───────────────────────────────────────────────────────────────────────          │
    │                  ││──────┬───────────┬─────┬───────────┬─────┬─────┬─────────────────────────────          │
    │i_data            ││ 0000 │FF00       │0A0B │FF00       │0CFF │00FF │000D                                   │
    │                  ││──────┴───────────┴─────┴───────────┴─────┴─────┴─────────────────────────────          │
    │i_valid           ││      ┌───────────────────────────────────────────────┐                                 │
    │                  ││──────┘                                               └───────────────────────          │
    │o_ready           ││      ┌───────────────────────┐     ┌─────────────────────────────────────────          │
    │                  ││──────┘                       └─────┘                                                   │
    │i_ready           ││      ┌───────────────────────────────────────────────┐                                 │
    │                  ││──────┘                                               └───────────────────────          │
    │                  ││──────────────────┬─────┬───────────┬─────┬───────────┬───────────────────────          │
    │o_data            ││ 0000             │FFFF │0A0B       │FFFF │0CFF       │FF0D                             │
    │                  ││──────────────────┴─────┴───────────┴─────┴───────────┴───────────────────────          │
    │o_valid           ││                  ┌───────────┐     ┌───────────┐     ┌─────┐                           │
    │                  ││──────────────────┘           └─────┘           └─────┘     └─────────────────          │
    │                  ││────────────┬───────────┬─────┬───────────────────────┬───────────────────────          │
    │buffered_byte     ││ 00         │FF         │0B   │FF                     │0D                               │
    │                  ││────────────┴───────────┴─────┴───────────────────────┴───────────────────────          │
    │                  ││──────┬───────────┬─────┬───────────┬─────┬───────────────────────────────────          │
    │data_1            ││ 00   │FF         │0A   │FF         │0C   │00                                           │
    │                  ││──────┴───────────┴─────┴───────────┴─────┴───────────────────────────────────          │
    │                  ││──────────────────┬─────┬───────────┬───────────┬─────────────────────────────          │
    │data_2            ││ 00               │0B   │00         │FF         │0D                                     │
    │                  ││──────────────────┴─────┴───────────┴───────────┴─────────────────────────────          │
    │gnd               ││                                                                                        │
    │                  ││──────────────────────────────────────────────────────────────────────────────          │
    │have_buffered_byte││            ┌─────┐           ┌─────┐           ┌─────┐                                 │
    │                  ││────────────┘     └───────────┘     └───────────┘     └───────────────────────          │
    │is_00_1           ││──────┐                                   ┌───────────────────────────────────          │
    │                  ││      └───────────────────────────────────┘                                             │
    │is_00_2           ││──────────────────┐     ┌───────────┐                                                   │
    │                  ││                  └─────┘           └─────────────────────────────────────────          │
    │is_ff_1           ││      ┌───────────┐     ┌───────────┐                                                   │
    │                  ││──────┘           └─────┘           └─────────────────────────────────────────          │
    │last_byte_was_ff  ││            ┌───────────┐     ┌───────────────────────┐                                 │
    │                  ││────────────┘           └─────┘                       └───────────────────────          │
    │remove_first_byte ││                                          ┌───────────┐                                 │
    │                  ││──────────────────────────────────────────┘           └───────────────────────          │
    │remove_second_byte││      ┌───────────┐     ┌───────────┐                                                   │
    │                  ││──────┘           └─────┘           └─────────────────────────────────────────          │
    │vdd               ││──────────────────────────────────────────────────────────────────────────────          │
    └──────────────────┘└────────────────────────────────────────────────────────────────────────────────────────┘ |}]
;;

let regression ?waves ?verbose ?(min = 252) size =
  (* put loads of data through, check result *)
  let d, q = create_entropy_bits ~min ~size in
  test ?waves ?verbose d q
;;

let%expect_test "regression" =
  let waves = regression 100 in
  Option.iter
    waves
    ~f:(Waveform.print ~wave_width:2 ~display_width:110 ~display_height:46);
  [%expect.unreachable]
  [@@expect.uncaught_exn
    {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  ("byte mismatch" (i 30) ("d.(i)" 0xff) ("r.(i)" 0xfd))
  Raised at Base__Error.raise in file "src/error.ml" (inlined), line 9, characters 14-30
  Called from Base__Error.raise_s in file "src/error.ml", line 10, characters 19-40
  Called from Hardcaml_jpeg_test__Test_filter_stuffed_bytes.test in file "video-coding/jpeg/hardcaml/test/test_filter_stuffed_bytes.ml", line 288, characters 6-88
  Called from Hardcaml_jpeg_test__Test_filter_stuffed_bytes.(fun) in file "video-coding/jpeg/hardcaml/test/test_filter_stuffed_bytes.ml", line 363, characters 14-28
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19 |}]
;;
