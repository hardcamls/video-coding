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
    {| (block.rle (((run 0) (value 0)) ((run 4) (value 10)) ((run 57) (value 0)))) |}]
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
       (((run 0) (value 0)) ((run 14) (value 10)) ((run 4) (value 20))
        ((run 42) (value 0)))) |}]
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
       (((run 0) (value 0)) ((run 0) (value 10)) ((run 3) (value 20))
        ((run 1) (value 30)) ((run 27) (value 60)) ((run 8) (value 40))
        ((run 16) (value 50)) ((run 1) (value 0)))) |}]
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
  if !pos <> 64 then raise_s [%message (!pos : int) (rle : Encoder.Rle.t list)];
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
  then raise_s [%message (block.quant : int array) (expected : int array)]
;;

let%expect_test "random tests" =
  for prob = 1 to 9 do
    let prob = 0.1 *. Float.of_int prob in
    for _ = 1 to 1_000 do
      test prob
    done
  done;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  ((block.quant
     (69 64 50 56 92 82 21 20 18 76 0 16 100 9 45 84 51 85 87 44 0 7 35 46 17
       28 4 87 0 27 27 71 17 14 0 63 33 0 8 0 56 21 24 11 39 74 30 74 15 85 41
       52 28 62 0 49 44 15 30 71 67 6 23 4))
    (expected
      (69 64 18 51 76 50 56 0 85 17 17 28 87 16 92 82 100 44 4 14 56 15 21 0 87
        0 9 21 20 45 7 0 63 24 85 44 15 41 11 33 27 35 84 46 27 0 39 52 30 71
        28 74 8 71 0 30 62 67 6 0 74 49 23 4)))
  Raised at Base__Error.raise in file "src/error.ml" (inlined), line 9, characters 14-30
  Called from Base__Error.raise_s in file "src/error.ml", line 10, characters 19-40
  Called from Hardcaml_jpeg_model_test__Test_rle.(fun) in file "video-coding/jpeg/model/test/test_rle.ml", line 127, characters 6-15
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19 |}]
;;
