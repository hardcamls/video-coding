open! Core
module Encoder = Hardcaml_jpeg_model.Encoder

let zz = Hardcaml_jpeg_model.Zigzag.forward

let%expect_test "all zeros" =
  let block = Encoder.Block.create () in
  Encoder.rle block;
  print_s [%message (block.rle : Encoder.Rle.t list)];
  [%expect {| (block.rle (((run 0) (value 0)) ((run 62) (value 0)))) |}]
;;

let%expect_test "dc only" =
  let block = Encoder.Block.create () in
  block.quant.(0) <- 10;
  Encoder.rle block;
  print_s [%message (block.rle : Encoder.Rle.t list)];
  [%expect {| (block.rle (((run 0) (value 10)) ((run 62) (value 0)))) |}]
;;

let%expect_test "single coef @ 1" =
  let block = Encoder.Block.create () in
  block.quant.(zz.(1)) <- 10;
  Encoder.rle block;
  print_s [%message (block.rle : Encoder.Rle.t list)];
  [%expect
    {| (block.rle (((run 0) (value 0)) ((run 0) (value 10)) ((run 61) (value 0)))) |}]
;;

let%expect_test "single coef @ 2" =
  let block = Encoder.Block.create () in
  block.quant.(zz.(2)) <- 10;
  Encoder.rle block;
  print_s [%message (block.rle : Encoder.Rle.t list)];
  [%expect
    {| (block.rle (((run 0) (value 0)) ((run 1) (value 10)) ((run 60) (value 0)))) |}]
;;

let%expect_test "single coef @ 62" =
  let block = Encoder.Block.create () in
  block.quant.(zz.(62)) <- 10;
  Encoder.rle block;
  print_s [%message (block.rle : Encoder.Rle.t list)];
  [%expect
    {| (block.rle (((run 0) (value 0)) ((run 61) (value 10)) ((run 0) (value 0)))) |}]
;;

let%expect_test "single coef @ 63" =
  let block = Encoder.Block.create () in
  block.quant.(zz.(63)) <- 10;
  Encoder.rle block;
  print_s [%message (block.rle : Encoder.Rle.t list)];
  [%expect {| (block.rle (((run 0) (value 0)) ((run 62) (value 10)))) |}]
;;

let%expect_test "two coefs @ 1, 63" =
  let block = Encoder.Block.create () in
  block.quant.(zz.(1)) <- 10;
  block.quant.(zz.(63)) <- 20;
  Encoder.rle block;
  print_s [%message (block.rle : Encoder.Rle.t list)];
  [%expect
    {| (block.rle (((run 0) (value 0)) ((run 0) (value 10)) ((run 61) (value 20)))) |}]
;;

let%expect_test "two coefs @ 5, 40" =
  let block = Encoder.Block.create () in
  block.quant.(zz.(5)) <- 10;
  block.quant.(zz.(40)) <- 20;
  Encoder.rle block;
  print_s [%message (block.rle : Encoder.Rle.t list)];
  [%expect
    {|
      (block.rle
       (((run 0) (value 0)) ((run 4) (value 10)) ((run 34) (value 20))
        ((run 22) (value 0)))) |}]
;;

let%expect_test "bunch of coefs" =
  let block = Encoder.Block.create () in
  block.quant.(zz.(1)) <- 10;
  block.quant.(zz.(2)) <- 20;
  block.quant.(zz.(10)) <- 30;
  block.quant.(zz.(30)) <- 40;
  block.quant.(zz.(55)) <- 50;
  block.quant.(zz.(56)) <- 60;
  Encoder.rle block;
  print_s [%message (block.rle : Encoder.Rle.t list)];
  [%expect
    {|
      (block.rle
       (((run 0) (value 0)) ((run 0) (value 10)) ((run 0) (value 20))
        ((run 7) (value 30)) ((run 19) (value 40)) ((run 24) (value 50))
        ((run 0) (value 60)) ((run 6) (value 0)))) |}]
;;

let rld (rle : Encoder.Rle.t list) =
  let a = Array.create ~len:64 (-1) in
  let pos = ref 0 in
  List.iter rle ~f:(fun { run; value } ->
      for i = 0 to run - 1 do
        a.(zz.(!pos + i)) <- 0
      done;
      a.(zz.(!pos + run)) <- value;
      pos := !pos + run + 1);
  assert (!pos = 64);
  a
;;

let random_block prob_zero =
  Array.init 64 ~f:(fun _ ->
      if Float.(Random.float 1. < prob_zero) then 0 else 1 + Random.int 100)
;;

let test prob_zero =
  let block = { (Encoder.Block.create ()) with quant = random_block prob_zero } in
  Encoder.rle block;
  let expected = rld block.rle in
  if not ([%compare.equal: int array] block.quant expected)
  then print_s [%message (block.quant : int array) (expected : int array)]
;;

let%expect_test "random tests" =
  for prob = 1 to 9 do
    let prob = 0.1 *. Float.of_int prob in
    for _ = 1 to 1_000 do
      test prob
    done
  done;
  [%expect {| |}]
;;
