(* Compare 3 different implementations of the forward and inverse DCT transforms. 
   
  The first is a basic matrix based 8x8 floating point transform (of type II and III) 
  as you can find defined on wikipedia or other sources.

  The second is a reference implementation which builds 8pt transforms from 4pt 
  transforms reducing the number of multipliers required.

  The last is a classic Chen DCT implementation suitable for software implementations.

  The hardware will be based on the 4pt decomposition.
*)

open Core
open Hardcaml_jpeg_model.Dct.Reference

let dump_float t =
  Util.iteri t ~f:(fun ~row:_ ~col v ->
      printf "%+.1f " v;
      if col = 7 then printf "\n")
;;

let dump_int t =
  Util.iteri t ~f:(fun ~row:_ ~col v ->
      printf "%+.4i " v;
      if col = 7 then printf "\n")
;;

let%expect_test "transform matrices are orthogonal" =
  let identity =
    Util.mul Eight_point.forward_transform_matrix Eight_point.inverse_transform_matrix
    |> Array.map ~f:(Array.map ~f:(Float.round_decimal ~decimal_digits:5))
  in
  dump_float identity;
  [%expect
    {|
    +1.0 +0.0 +0.0 +0.0 +0.0 +0.0 +0.0 +0.0
    +0.0 +1.0 +0.0 +0.0 +0.0 +0.0 +0.0 +0.0
    +0.0 +0.0 +1.0 +0.0 +0.0 +0.0 +0.0 +0.0
    +0.0 +0.0 +0.0 +1.0 +0.0 +0.0 +0.0 +0.0
    +0.0 +0.0 +0.0 +0.0 +1.0 +0.0 +0.0 +0.0
    +0.0 +0.0 +0.0 +0.0 +0.0 +1.0 +0.0 +0.0
    +0.0 +0.0 +0.0 +0.0 +0.0 +0.0 +1.0 +0.0
    +0.0 +0.0 +0.0 +0.0 +0.0 +0.0 +0.0 +1.0 |}]
;;

(* Input reference data for testing *)
let refdata = Array.init 8 ~f:(fun _ -> Array.init 8 ~f:(fun _ -> Random.int 100 - 50))

let forward_chen t =
  let o = Array.to_list t |> Array.concat in
  Hardcaml_jpeg_model.Dct.Chen.forward_8x8 o;
  (* The Chen transform is scaled by 4. *)
  Util.init (fun ~row ~col ->
      let x = o.((row * 8) + col) in
      if x > 0 then (x + 2) / 4 else (x - 2) / 4)
;;

let inverse_chen t =
  let o = Array.to_list t |> Array.concat in
  Hardcaml_jpeg_model.Dct.Chen.inverse_8x8 o;
  (* The Chen transform is scaled by 4. *)
  Util.init (fun ~row ~col -> o.((row * 8) + col))
;;

let forward_ref t =
  Eight_point.forward_transform (Util.map t ~f:Float.of_int)
  |> Util.map ~f:(Fn.compose Float.to_int Float.round_nearest)
;;

let inverse_ref t =
  Eight_point.inverse_transform (Util.map t ~f:Float.of_int)
  |> Util.map ~f:(Fn.compose Float.to_int Float.round_nearest)
;;

let diff = Util.map2 ~f:(fun a b -> Int.abs (a - b))

let%expect_test "compare forward chen and reference transforms" =
  let chen = forward_chen refdata in
  let reference = forward_ref refdata in
  let diff = diff chen reference in
  dump_int diff;
  [%expect
    {|
    +0001 +0000 +0000 +0000 +0000 +0000 +0001 +0001
    +0001 +0000 +0000 +0001 +0000 +0001 +0001 +0000
    +0000 +0000 +0000 +0001 +0000 +0000 +0000 +0000
    +0001 +0000 +0000 +0000 +0000 +0000 +0000 +0000
    +0001 +0000 +0000 +0000 +0001 +0000 +0000 +0000
    +0001 +0000 +0000 +0001 +0000 +0000 +0001 +0000
    +0000 +0000 +0000 +0000 +0000 +0000 +0001 +0000
    +0000 +0001 +0001 +0000 +0000 +0000 +0000 +0001 |}]
;;

let%expect_test "compare inverse chen and reference transforms" =
  let fdct = forward_ref refdata in
  let chen = inverse_chen fdct in
  let reference = inverse_ref fdct in
  let diff = diff chen reference in
  dump_int diff;
  [%expect
    {|
    +0000 +0000 +0000 +0000 +0000 +0000 +0000 +0000
    +0000 +0000 +0000 +0000 +0000 +0000 +0000 +0000
    +0000 +0000 +0000 +0000 +0000 +0000 +0000 +0000
    +0000 +0000 +0000 +0000 +0000 +0000 +0000 +0000
    +0000 +0000 +0000 +0000 +0000 +0000 +0000 +0000
    +0000 +0000 +0000 +0000 +0000 +0000 +0000 +0000
    +0000 +0000 +0000 +0000 +0000 +0000 +0000 +0000
    +0000 +0000 +0000 +0001 +0000 +0000 +0000 +0000 |}]
;;

let forward_using_4pt t =
  Using_four_point.forward_transform (Util.map t ~f:Float.of_int)
  |> Util.map ~f:(Fn.compose Float.to_int Float.round_nearest)
;;

let%expect_test "compare forward transform built from sub-4point transforms to reference" =
  let from_4pt = forward_using_4pt refdata in
  let reference = forward_ref refdata in
  let diff = diff from_4pt reference in
  dump_int diff;
  [%expect
    {|
    +0000 +0000 +0000 +0000 +0000 +0000 +0000 +0000
    +0000 +0000 +0000 +0000 +0000 +0000 +0000 +0000
    +0000 +0000 +0000 +0000 +0000 +0000 +0000 +0000
    +0000 +0000 +0000 +0000 +0000 +0000 +0000 +0000
    +0000 +0000 +0000 +0000 +0000 +0000 +0000 +0000
    +0000 +0000 +0000 +0000 +0000 +0000 +0000 +0000
    +0000 +0000 +0000 +0000 +0000 +0000 +0000 +0000
    +0000 +0000 +0000 +0000 +0000 +0000 +0000 +0000 |}]
;;

let inverse_using_4pt t =
  Using_four_point.inverse_transform (Util.map t ~f:Float.of_int)
  |> Util.map ~f:(Fn.compose Float.to_int Float.round_nearest)
;;

let%expect_test "compare inverse transform built from sub-4point transforms to reference" =
  let fdct = forward_ref refdata in
  let from_4pt = inverse_using_4pt fdct in
  let reference = inverse_ref fdct in
  let diff = diff from_4pt reference in
  dump_int diff;
  [%expect
    {|
    +0000 +0000 +0000 +0000 +0000 +0000 +0000 +0000
    +0000 +0000 +0000 +0000 +0000 +0000 +0000 +0000
    +0000 +0000 +0000 +0000 +0000 +0000 +0000 +0000
    +0000 +0000 +0000 +0000 +0000 +0000 +0000 +0000
    +0000 +0000 +0000 +0000 +0000 +0000 +0000 +0000
    +0000 +0000 +0000 +0000 +0000 +0000 +0000 +0000
    +0000 +0000 +0000 +0000 +0000 +0000 +0000 +0000
    +0000 +0000 +0000 +0000 +0000 +0000 +0000 +0000 |}]
;;
