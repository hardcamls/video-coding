open Core

(* Here we see the maximum range is [-0.5 < x < 0.5] *)
let%expect_test "min and max idct coefs" =
  let calc matrix min_or_max =
    Array.fold matrix ~init:0. ~f:(fun acc ->
        Array.fold ~init:acc ~f:(fun acc m -> min_or_max m acc))
  in
  let fdct =
    calc Hardcaml_jpeg_model.Dct.Reference.Eight_point.forward_transform_matrix
  in
  let max = fdct Float.max in
  let min = fdct Float.min in
  print_s [%message "fdct coefficient range" (max : float) (min : float)];
  [%expect
    {|
    ("fdct coefficient range" (max 0.49039264020161533)
     (min -0.49039264020161533)) |}];
  let idct =
    calc Hardcaml_jpeg_model.Dct.Reference.Eight_point.inverse_transform_matrix
  in
  let max = idct Float.max in
  let min = idct Float.min in
  print_s [%message "idct coefficient range" (max : float) (min : float)];
  [%expect
    {|
    ("idct coefficient range" (max 0.49039264020161533)
     (min -0.49039264020161533)) |}]
;;

(* Play with the required fixed precision.  

  10 bits produces an error of around +/-1.
  
  12 bits is about +/- 0.25

  14 bits is about +/- 0.1, except in the DC coefficient where we are seeing 
  diminishing returns.
 *)

let eval_in_fixed_point ~coefs ~inputs ~fixed_prec =
  let fixed_scale = Float.(2. ** of_int fixed_prec) in
  let to_fixed f = Float.(to_int (round_nearest (f * fixed_scale))) in
  let of_fixed i = Float.(of_int i / fixed_scale) in
  let coefs = Array.map coefs ~f:to_fixed in
  let result = Array.map2_exn inputs coefs ~f:( * ) |> Array.fold ~init:0 ~f:( + ) in
  let float_result = of_fixed result in
  result, float_result
;;

let%expect_test "fixed sum" =
  let inputs = Array.init 8 ~f:(fun _ -> Random.int 4000 - 2000) in
  let row i =
    let row =
      Hardcaml_jpeg_model.Dct.Reference.Eight_point.forward_transform_matrix.(i)
    in
    let float_result =
      Array.map2_exn (Array.map inputs ~f:Float.of_int) row ~f:Float.( * )
      |> Array.fold ~init:0. ~f:Float.( + )
    in
    let _, fixed_result = eval_in_fixed_point ~coefs:row ~inputs ~fixed_prec:12 in
    float_result, fixed_result
  in
  let results = Array.init 8 ~f:row in
  print_s [%message (results : (float * float) array)];
  [%expect
    {|
    (results
     ((2593.314120001663 2593.037109375) (-428.79891571033772 -428.818603515625)
      (1079.1220528682402 1079.39453125) (1377.8954860558251 1378.12548828125)
      (1260.417837465021 1260.283203125) (160.39849304801066 160.163330078125)
      (-1946.7233611927106 -1946.5771484375)
      (-1246.7619431232808 -1246.8330078125))) |}];
  let error = Array.map results ~f:(fun (a, b) -> Float.(abs (a - b))) in
  print_s [%message (error : float array)];
  [%expect
    {|
    (error
     (0.27701062666301368 0.019687805287276205 0.27247838175981087
      0.23000222542486881 0.13463434002096619 0.2351629698856641
      0.14621275521062671 0.071064689219156207)) |}]
;;
