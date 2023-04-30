open! Core
module Quant = Hardcaml_jpeg_model.Quant_tables

let%expect_test "1" =
  print_s [%message (Quant.scale Quant.luma 1 : int array)];
  [%expect
    {|
    ("Quant.scale Quant.luma 1"
     (255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
      255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
      255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255
      255 255 255 255 255 255 255)) |}]
;;

let%expect_test "25" =
  print_s [%message (Quant.scale Quant.luma 25 : int array)];
  [%expect
    {|
    ("Quant.scale Quant.luma 25"
     (32 22 20 32 48 80 102 122 24 24 28 38 52 116 120 110 28 26 32 48 80 114 138
      112 28 34 44 58 102 174 160 124 36 44 74 112 136 218 206 154 48 70 110 128
      162 208 226 184 98 128 156 174 206 242 240 202 144 184 190 196 224 200 206
      198)) |}]
;;

let%expect_test "50" =
  print_s [%message (Quant.scale Quant.luma 50 : int array)];
  [%expect
    {|
    ("Quant.scale Quant.luma 50"
     (16 11 10 16 24 40 51 61 12 12 14 19 26 58 60 55 14 13 16 24 40 57 69 56 14
      17 22 29 51 87 80 62 18 22 37 56 68 109 103 77 24 35 55 64 81 104 113 92 49
      64 78 87 103 121 120 101 72 92 95 98 112 100 103 99)) |}]
;;

let%expect_test "75" =
  print_s [%message (Quant.scale Quant.luma 75 : int array)];
  [%expect
    {|
    ("Quant.scale Quant.luma 75"
     (8 6 5 8 12 20 26 31 6 6 7 10 13 29 30 28 7 7 8 12 20 29 35 28 7 9 11 15 26
      44 40 31 9 11 19 28 34 55 52 39 12 18 28 32 41 52 57 46 25 32 39 44 52 61
      60 51 36 46 48 49 56 50 52 50)) |}]
;;

let%expect_test "95" =
  print_s [%message (Quant.scale Quant.luma 95 : int array)];
  [%expect
    {|
    ("Quant.scale Quant.luma 95"
     (2 1 1 2 2 4 5 6 1 1 1 2 3 6 6 6 1 1 2 2 4 6 7 6 1 2 2 3 5 9 8 6 2 2 4 6 7
      11 10 8 2 4 6 6 8 10 11 9 5 6 8 9 10 12 12 10 7 9 10 10 11 10 10 10)) |}]
;;

let%expect_test "100" =
  print_s [%message (Quant.scale Quant.luma 100 : int array)];
  [%expect
    {|
    ("Quant.scale Quant.luma 100"
     (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
      1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) |}]
;;
