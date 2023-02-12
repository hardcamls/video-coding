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

type eval_result =
  { unscaled : int
  ; scaled_and_clipped : int
  ; floating : float
  }
[@@deriving sexp_of]

let scale_and_clip x ~fixed_prec =
  let half = 1 lsl (fixed_prec - 1) in
  let floor x = x asr fixed_prec in
  let ceil x = floor (x + ((1 lsl fixed_prec) - 1)) in
  let x = if x < 0 then ceil (x - half) else floor (x + half) in
  if x > 127 then 127 else if x < -128 then -128 else x
;;

let%expect_test "test scaling" =
  let prec = 3 in
  for i = -16 to 15 do
    let f = Float.(of_int i / (2. ** of_int prec)) in
    Stdio.printf
      "%4i %4i   %+.4f %s [%s]\n"
      i
      (scale_and_clip i ~fixed_prec:prec)
      f
      (if (i lsr (prec - 1)) land 1 <> 0 then "t" else "f")
      (Hardcaml.Bits.of_int ~width:5 i |> Hardcaml.Bits.to_string)
  done;
  [%expect
    {|
      -16   -2   -2.0000 f [10000]
      -15   -2   -1.8750 f [10001]
      -14   -2   -1.7500 f [10010]
      -13   -2   -1.6250 f [10011]
      -12   -2   -1.5000 t [10100]
      -11   -1   -1.3750 t [10101]
      -10   -1   -1.2500 t [10110]
       -9   -1   -1.1250 t [10111]
       -8   -1   -1.0000 f [11000]
       -7   -1   -0.8750 f [11001]
       -6   -1   -0.7500 f [11010]
       -5   -1   -0.6250 f [11011]
       -4   -1   -0.5000 t [11100]
       -3    0   -0.3750 t [11101]
       -2    0   -0.2500 t [11110]
       -1    0   -0.1250 t [11111]
        0    0   +0.0000 f [00000]
        1    0   +0.1250 f [00001]
        2    0   +0.2500 f [00010]
        3    0   +0.3750 f [00011]
        4    1   +0.5000 t [00100]
        5    1   +0.6250 t [00101]
        6    1   +0.7500 t [00110]
        7    1   +0.8750 t [00111]
        8    1   +1.0000 f [01000]
        9    1   +1.1250 f [01001]
       10    1   +1.2500 f [01010]
       11    1   +1.3750 f [01011]
       12    2   +1.5000 t [01100]
       13    2   +1.6250 t [01101]
       14    2   +1.7500 t [01110]
       15    2   +1.8750 t [01111] |}]
;;

let eval_in_fixed_point ~coefs ~inputs ~fixed_prec =
  let fixed_scale = Float.(2. ** of_int fixed_prec) in
  let to_fixed f = Float.(to_int (round_nearest (f * fixed_scale))) in
  let of_fixed i = Float.(of_int i / fixed_scale) in
  let coefs = Array.map coefs ~f:to_fixed in
  let unscaled = Array.map2_exn inputs coefs ~f:( * ) |> Array.fold ~init:0 ~f:( + ) in
  let floating = of_fixed unscaled in
  { unscaled; scaled_and_clipped = scale_and_clip unscaled ~fixed_prec; floating }
;;

let%expect_test "fixed sum" =
  let inputs = Array.init 8 ~f:(fun _ -> Random.int 400 - 200) in
  let row i =
    let row =
      Hardcaml_jpeg_model.Dct.Reference.Eight_point.forward_transform_matrix.(i)
    in
    let float_result =
      Array.map2_exn (Array.map inputs ~f:Float.of_int) row ~f:Float.( * )
      |> Array.fold ~init:0. ~f:Float.( + )
    in
    eval_in_fixed_point ~coefs:row ~inputs ~fixed_prec:12, float_result
  in
  let results = Array.init 8 ~f:row in
  print_s [%message (results : (eval_result * float) array)];
  [%expect
    {|
    (results
     ((((unscaled -383720) (scaled_and_clipped -94) (floating -93.681640625))
       -93.691648507217536)
      (((unscaled 692359) (scaled_and_clipped 127) (floating 169.032958984375))
       169.00416588982239)
      (((unscaled -542000) (scaled_and_clipped -128) (floating -132.32421875))
       -132.33331133947192)
      (((unscaled 437202) (scaled_and_clipped 107) (floating 106.73876953125))
       106.75520931213394)
      (((unscaled -50680) (scaled_and_clipped -12) (floating -12.373046875))
       -12.374368670764497)
      (((unscaled -323971) (scaled_and_clipped -79) (floating -79.094482421875))
       -79.058145677711138)
      (((unscaled -275580) (scaled_and_clipped -67) (floating -67.2802734375))
       -67.26176261391268)
      (((unscaled -831428) (scaled_and_clipped -128) (floating -202.9853515625))
       -202.97346328673086))) |}];
  let error = Array.map results ~f:(fun (a, b) -> Float.(abs (a.floating - b))) in
  print_s [%message (error : float array)];
  [%expect
    {|
    (error
     (0.010007882217536235 0.028793094552611365 0.0090925894719191547
      0.01643978088394249 0.0013217957644968692 0.036336744163861567
      0.018510823587320147 0.01188827576913809)) |}]
;;
