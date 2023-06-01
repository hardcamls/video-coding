open! Core
open Hardcaml
module Quant = Hardcaml_jpeg.Quant

let%expect_test "rounding - tie away from zero" =
  let sft = 2 in
  let div = 1 lsl sft in
  let half = div / 2 in
  for i = -11 to 11 do
    let isft = if i < 0 then (i + half - 1) asr sft else (i + half) asr sft in
    let idiv = if i < 0 then (i - half) / div else (i + half) / div in
    print_s [%message (i : int) (idiv : int) (isft : int)]
  done;
  [%expect
    {|
    ((i -11) (idiv -3) (isft -3))
    ((i -10) (idiv -3) (isft -3))
    ((i -9) (idiv -2) (isft -2))
    ((i -8) (idiv -2) (isft -2))
    ((i -7) (idiv -2) (isft -2))
    ((i -6) (idiv -2) (isft -2))
    ((i -5) (idiv -1) (isft -1))
    ((i -4) (idiv -1) (isft -1))
    ((i -3) (idiv -1) (isft -1))
    ((i -2) (idiv -1) (isft -1))
    ((i -1) (idiv 0) (isft 0))
    ((i 0) (idiv 0) (isft 0))
    ((i 1) (idiv 0) (isft 0))
    ((i 2) (idiv 1) (isft 1))
    ((i 3) (idiv 1) (isft 1))
    ((i 4) (idiv 1) (isft 1))
    ((i 5) (idiv 1) (isft 1))
    ((i 6) (idiv 2) (isft 2))
    ((i 7) (idiv 2) (isft 2))
    ((i 8) (idiv 2) (isft 2))
    ((i 9) (idiv 2) (isft 2))
    ((i 10) (idiv 3) (isft 3))
    ((i 11) (idiv 3) (isft 3)) |}]
;;

(* Let's have a look at precision *)

(* Reference division using floats *)
let float_quant d q =
  let pos = d >= 0 in
  let d = Float.of_int d in
  let q = Float.of_int q in
  let r = d /. q in
  if pos then Float.to_int (r +. 0.5) else Float.to_int (r -. 0.5)
;;

(* Software quantiser using division *)
let int_quant d q = if d >= 0 then (d + (q / 2)) / q else (d - (q / 2)) / q

(* Fixed point implementation using multiplication by 1/q *)
let fixed_quant d q =
  let prec = Quant.quant_coef_bits - 1 in
  let half = 1 lsl (prec - 1) in
  let q = (1 lsl prec) / q in
  if d < 0 then ((d * q) + half - 1) asr prec else ((d * q) + half) asr prec
;;

let test_range ?(verbose = false) mn mx =
  for q = 1 to 255 do
    let max_error_float_int = ref 0 in
    let max_error_float_fixed = ref 0 in
    let total_error_float_fixed = ref 0 in
    for d = mn to mx do
      let fixed = fixed_quant d q in
      let int = int_quant d q in
      let float = float_quant d q in
      max_error_float_int := max !max_error_float_int (Int.abs (float - int));
      max_error_float_fixed := max !max_error_float_fixed (Int.abs (float - fixed));
      total_error_float_fixed := !total_error_float_fixed + Int.abs (float - fixed)
    done;
    let msg () =
      [%message
        (!max_error_float_int : int)
          (!max_error_float_fixed : int)
          (!total_error_float_fixed : int)]
    in
    if verbose then print_s (msg ());
    if !max_error_float_int > 1 || !max_error_float_fixed > 1 then raise_s (msg ())
  done
;;

let%expect_test "negative" =
  test_range (-2048) 0;
  [%expect {||}]
;;

let%expect_test "positive" =
  test_range (-2048) 0;
  [%expect {||}]
;;

(* Hardware tests *)

module Sim = Cyclesim.With_interface (Quant.I) (Quant.O)

let ( <--. ) a b = a := Bits.of_int ~width:(Bits.width !a) b

(* XX I think there is a bug when the quant factor is [1].  
   The inverse scaled multiplier is larger than the port width 
   for the quantiser. *)

let%expect_test "Test quantiser using random coefs and quant table entries" =
  let num_tests = 10_000 in
  let sim = Sim.create (Quant.create (Scope.create ())) in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  inputs.clocking.clear := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clocking.clear := Bits.gnd;
  let qtab =
    Array.init 64 ~f:(fun i ->
        if i < 4 then i + 1 else if i > 60 then i + (252 - 60) else 1 + Random.int 255)
  in
  print_s [%message (qtab : int array)];
  [%expect
    {|
    (qtab
     (1 2 3 4 160 163 14 196 184 34 223 99 80 244 242 11 232 141 202 24 46 37 221
      7 221 28 217 130 69 76 118 107 211 56 69 227 99 95 11 119 138 20 88 184 49
      216 211 100 251 160 129 119 77 141 216 162 130 145 165 227 227 253 254 255)) |}];
  let qdata = Array.init num_tests ~f:(fun _ -> Random.int 64, Random.int 4096 - 2048) in
  (* load quantisation table *)
  for i = 0 to 63 do
    inputs.quant.quant <--. Quant.one_over_quant_coef qtab.(i);
    inputs.quant.write <--. 1;
    inputs.quant.address <--. i;
    Cyclesim.cycle sim
  done;
  inputs.quant.write <--. 0;
  Cyclesim.cycle sim;
  (* Run through test data  *)
  inputs.enable := Bits.vdd;
  let results = ref [] in
  let cycle () =
    Cyclesim.cycle sim;
    if Bits.to_bool !(outputs.quant_coef_write)
    then
      results
        := (Bits.to_int !(outputs.quant_coef_address), Bits.to_sint !(outputs.quant_coef))
           :: !results
  in
  Array.iter qdata ~f:(fun (addr, data) ->
      inputs.dct_coef <--. data;
      inputs.dct_coef_write <--. 1;
      inputs.dct_coef_address <--. addr;
      cycle ());
  inputs.dct_coef_write <--. 0;
  (* flush *)
  for _ = 0 to 10 do
    cycle ()
  done;
  (* check *)
  assert (List.length !results = Array.length qdata);
  (* print_s [%message (List.rev !results : (int * int) list) (qdata : (int * int) array)]; *)
  List.iteri (List.rev !results) ~f:(fun idx (addr, result) ->
      let expected_addr, d = qdata.(idx) in
      let expected_result = fixed_quant d qtab.(expected_addr) in
      if expected_addr <> addr || expected_result <> result
      then
        raise_s
          [%message
            (d : int)
              (qtab.(expected_addr) : int)
              (expected_addr : int)
              (addr : int)
              (expected_result : int)
              (result : int)])
;;

let%expect_test "bug hunt" =
  let test d q =
    let expected = fixed_quant d q in
    let d = Bits.of_int ~width:Quant.dct_coef_bits d in
    let q = Bits.of_int ~width:Quant.quant_coef_bits (Quant.one_over_quant_coef q) in
    let doq = Quant.multiply (module Bits) d q in
    let rounded = Quant.round (module Bits) doq in
    print_s
      [%message
        (d : Bits.t)
          (q : Bits.t)
          (doq : Bits.t)
          (rounded : Bits.t)
          (Bits.to_sint rounded : int)
          (expected : int)]
  in
  test (-188) 2;
  [%expect
    {|
    ((d 111101000100) (q 0100000000000) (doq 11111110100010000000000000)
     (rounded 111110100010) ("Bits.to_sint rounded" -94) (expected -94)) |}];
  test 709 1;
  [%expect
    {|
    ((d 001011000101) (q 1000000000000) (doq 00001011000101000000000000)
     (rounded 001011000101) ("Bits.to_sint rounded" 709) (expected 709)) |}]
;;
