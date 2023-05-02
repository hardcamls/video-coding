open! Core
module Encoder = Hardcaml_jpeg_model.Encoder

let%expect_test "all zeros" =
  let block = Encoder.Block.create false in
  Encoder.rle block;
  print_s [%message (block.rle : Encoder.Rle.t list)];
  [%expect {| (block.rle (((run 0) (value 0)) ((run 62) (value 0)))) |}]
;;

let%expect_test "dc only" =
  let block = Encoder.Block.create false in
  block.quant.(0) <- 10;
  Encoder.rle block;
  print_s [%message (block.rle : Encoder.Rle.t list)];
  [%expect {| (block.rle (((run 0) (value 10)) ((run 62) (value 0)))) |}]
;;

let%expect_test "single coef @ 1" =
  let block = Encoder.Block.create false in
  block.quant.(1) <- 10;
  Encoder.rle block;
  print_s [%message (block.rle : Encoder.Rle.t list)];
  [%expect
    {| (block.rle (((run 0) (value 0)) ((run 0) (value 10)) ((run 61) (value 0)))) |}]
;;

let%expect_test "single coef @ 2" =
  let block = Encoder.Block.create false in
  block.quant.(2) <- 10;
  Encoder.rle block;
  print_s [%message (block.rle : Encoder.Rle.t list)];
  [%expect
    {| (block.rle (((run 0) (value 0)) ((run 1) (value 10)) ((run 60) (value 0)))) |}]
;;

let%expect_test "single coef @ 62" =
  let block = Encoder.Block.create false in
  block.quant.(62) <- 10;
  Encoder.rle block;
  print_s [%message (block.rle : Encoder.Rle.t list)];
  [%expect
    {| (block.rle (((run 0) (value 0)) ((run 61) (value 10)) ((run 0) (value 0)))) |}]
;;

let%expect_test "single coef @ 63" =
  let block = Encoder.Block.create false in
  block.quant.(63) <- 10;
  Encoder.rle block;
  print_s [%message (block.rle : Encoder.Rle.t list)];
  [%expect {| (block.rle (((run 0) (value 0)) ((run 62) (value 10)))) |}]
;;

let%expect_test "two coefs @ 1, 63" =
  let block = Encoder.Block.create false in
  block.quant.(1) <- 10;
  block.quant.(63) <- 20;
  Encoder.rle block;
  print_s [%message (block.rle : Encoder.Rle.t list)];
  [%expect
    {| (block.rle (((run 0) (value 0)) ((run 0) (value 10)) ((run 61) (value 20)))) |}]
;;

let%expect_test "two coefs @ 5, 40" =
  let block = Encoder.Block.create false in
  block.quant.(5) <- 10;
  block.quant.(40) <- 20;
  Encoder.rle block;
  print_s [%message (block.rle : Encoder.Rle.t list)];
  [%expect
    {|
      (block.rle
       (((run 0) (value 0)) ((run 4) (value 10)) ((run 34) (value 20))
        ((run 22) (value 0)))) |}]
;;

let%expect_test "bunch of coefs" =
  let block = Encoder.Block.create false in
  block.quant.(1) <- 10;
  block.quant.(2) <- 20;
  block.quant.(10) <- 30;
  block.quant.(30) <- 40;
  block.quant.(55) <- 50;
  block.quant.(56) <- 60;
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
        a.(!pos + i) <- 0
      done;
      a.(!pos + run) <- value;
      pos := !pos + run + 1);
  if !pos <> 64 then raise_s [%message (!pos : int) (rle : Encoder.Rle.t list)];
  a
;;

let random_block prob_zero =
  Array.init 64 ~f:(fun _ ->
      if Float.(Random.float 1. < prob_zero) then 0 else 1 + Random.int 100)
;;

let test prob_zero =
  let block = { (Encoder.Block.create false) with quant = random_block prob_zero } in
  Encoder.rle block;
  let expected = rld block.rle in
  if not ([%compare.equal: int array] block.quant expected)
  then raise_s [%message (block.quant : int array) (expected : int array)]
;;

let%expect_test "random tests" =
  for prob = 1 to 9 do
    let prob = 0.1 *. Float.of_int prob in
    for _ = 1 to 1_000 do
      test prob
    done
  done;
  [%expect{||}]
;;
