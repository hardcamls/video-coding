open! Core

include struct
  open Hardcaml_jpeg_model
  module Encoder = Encoder
  module Model = Model
  module Tables = Tables
end

let%expect_test "range and encoded size" =
  for i = 0 to 11 do
    let lo = 1 lsl (i - 1) in
    let hi = (1 lsl i) - 1 in
    let size_lo = Encoder.size lo in
    let size_hi = Encoder.size hi in
    print_s [%message (i : int) (lo : int) (hi : int) (size_lo : int) (size_hi : int)]
  done;
  [%expect
    {|
    ((i 0) (lo 0) (hi 0) (size_lo 0) (size_hi 0))
    ((i 1) (lo 1) (hi 1) (size_lo 1) (size_hi 1))
    ((i 2) (lo 2) (hi 3) (size_lo 2) (size_hi 2))
    ((i 3) (lo 4) (hi 7) (size_lo 3) (size_hi 3))
    ((i 4) (lo 8) (hi 15) (size_lo 4) (size_hi 4))
    ((i 5) (lo 16) (hi 31) (size_lo 5) (size_hi 5))
    ((i 6) (lo 32) (hi 63) (size_lo 6) (size_hi 6))
    ((i 7) (lo 64) (hi 127) (size_lo 7) (size_hi 7))
    ((i 8) (lo 128) (hi 255) (size_lo 8) (size_hi 8))
    ((i 9) (lo 256) (hi 511) (size_lo 9) (size_hi 9))
    ((i 10) (lo 512) (hi 1023) (size_lo 10) (size_hi 10))
    ((i 11) (lo 1024) (hi 2047) (size_lo 11) (size_hi 11)) |}]
;;

let%expect_test "test magnitude encoding" =
  let mag value =
    let size = Encoder.size value in
    let emag = Encoder.magnitude ~size value in
    let dmag = Model.For_testing.mag size emag in
    print_s [%message (value : int) (size : int) (emag : int) (dmag : int)]
  in
  for i = -15 to 15 do
    mag i
  done;
  [%expect
    {|
    ((value -15) (size 4) (emag 0) (dmag -15))
    ((value -14) (size 4) (emag 1) (dmag -14))
    ((value -13) (size 4) (emag 2) (dmag -13))
    ((value -12) (size 4) (emag 3) (dmag -12))
    ((value -11) (size 4) (emag 4) (dmag -11))
    ((value -10) (size 4) (emag 5) (dmag -10))
    ((value -9) (size 4) (emag 6) (dmag -9))
    ((value -8) (size 4) (emag 7) (dmag -8))
    ((value -7) (size 3) (emag 0) (dmag -7))
    ((value -6) (size 3) (emag 1) (dmag -6))
    ((value -5) (size 3) (emag 2) (dmag -5))
    ((value -4) (size 3) (emag 3) (dmag -4))
    ((value -3) (size 2) (emag 0) (dmag -3))
    ((value -2) (size 2) (emag 1) (dmag -2))
    ((value -1) (size 1) (emag 0) (dmag -1))
    ((value 0) (size 0) (emag 0) (dmag 0))
    ((value 1) (size 1) (emag 1) (dmag 1))
    ((value 2) (size 2) (emag 2) (dmag 2))
    ((value 3) (size 2) (emag 3) (dmag 3))
    ((value 4) (size 3) (emag 4) (dmag 4))
    ((value 5) (size 3) (emag 5) (dmag 5))
    ((value 6) (size 3) (emag 6) (dmag 6))
    ((value 7) (size 3) (emag 7) (dmag 7))
    ((value 8) (size 4) (emag 8) (dmag 8))
    ((value 9) (size 4) (emag 9) (dmag 9))
    ((value 10) (size 4) (emag 10) (dmag 10))
    ((value 11) (size 4) (emag 11) (dmag 11))
    ((value 12) (size 4) (emag 12) (dmag 12))
    ((value 13) (size 4) (emag 13) (dmag 13))
    ((value 14) (size 4) (emag 14) (dmag 14))
    ((value 15) (size 4) (emag 15) (dmag 15)) |}]
;;
