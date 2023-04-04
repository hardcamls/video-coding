open Core

(* We have seen a difference in the matrices generated in x86 (amd) and ARM processors. 

  This has been leading to some changes in the tests.  Print them out here so we can
  further debug and decide what to do about it.

  Digging in a bit we find the following:

  Float.cos 10.2101761241668285;;

  This results in different values on mac and linux.
*)

let%expect_test "forward" =
  let open Hardcaml_jpeg_model.Dct in
  let forward = Floating_point.Eight_point.forward_transform_matrix in
  print_s [%message (forward : float Matrix8x8.t)];
  let forward = Matrix8x8.map forward ~f:Int64.bits_of_float in
  print_s [%message (forward : Int64.Hex.t Matrix8x8.t)];
  [%expect
    {|
    (forward
     ((0.35355339059327373 0.35355339059327373 0.35355339059327373
       0.35355339059327373 0.35355339059327373 0.35355339059327373
       0.35355339059327373 0.35355339059327373)
      (0.49039264020161522 0.41573480615127262 0.27778511650980114
       0.097545161008064166 -0.0975451610080641 -0.277785116509801
       -0.41573480615127267 -0.49039264020161522)
      (0.46193976625564337 0.19134171618254492 -0.19134171618254486
       -0.46193976625564337 -0.46193976625564342 -0.19134171618254517
       0.191341716182545 0.46193976625564326)
      (0.41573480615127262 -0.0975451610080641 -0.49039264020161522
       -0.27778511650980109 0.27778511650980092 0.49039264020161533
       0.097545161008063958 -0.41573480615127212)
      (0.35355339059327379 -0.35355339059327373 -0.35355339059327384
       0.35355339059327368 0.35355339059327384 -0.35355339059327334
       -0.35355339059327356 0.35355339059327329)
      (0.27778511650980114 -0.49039264020161522 0.097545161008064152
       0.41573480615127273 -0.41573480615127256 -0.097545161008064887
       0.49039264020161516 -0.27778511650980076)
      (0.19134171618254492 -0.46193976625564342 0.46193976625564326
       -0.19134171618254495 -0.19134171618254528 0.4619397662556437
       -0.46193976625564354 0.19134171618254314)
      (0.097545161008064166 -0.27778511650980109 0.41573480615127273
       -0.49039264020161533 0.49039264020161522 -0.415734806151272
       0.2777851165098022 -0.097545161008062542)))
    (forward
     ((0x3fd6a09e667f3bcc 0x3fd6a09e667f3bcc 0x3fd6a09e667f3bcc
       0x3fd6a09e667f3bcc 0x3fd6a09e667f3bcc 0x3fd6a09e667f3bcc
       0x3fd6a09e667f3bcc 0x3fd6a09e667f3bcc)
      (0x3fdf6297cff75cb0 0x3fda9b66290ea1a3 0x3fd1c73b39ae68c9
       0x3fb8f8b83c69a60d -0x40470747c39659f8 -0x402e38c4c651973a
       -0x40256499d6f15e5c -0x40209d683008a350)
      (0x3fdd906bcf328d46 0x3fc87de2a6aea964 -0x4037821d5951569e
       -0x40226f9430cd72ba -0x40226f9430cd72b9 -0x4037821d59515693
       0x3fc87de2a6aea967 0x3fdd906bcf328d44)
      (0x3fda9b66290ea1a3 -0x40470747c39659f8 -0x40209d683008a350
       -0x402e38c4c6519738 0x3fd1c73b39ae68c5 0x3fdf6297cff75cb2
       0x3fb8f8b83c69a5fe -0x40256499d6f15e66)
      (0x3fd6a09e667f3bcd -0x40295f619980c434 -0x40295f619980c432
       0x3fd6a09e667f3bcb 0x3fd6a09e667f3bce -0x40295f619980c43b
       -0x40295f619980c437 0x3fd6a09e667f3bc4)
      (0x3fd1c73b39ae68c9 -0x40209d683008a350 0x3fb8f8b83c69a60c
       0x3fda9b66290ea1a5 -0x40256499d6f15e5e -0x40470747c39659bf
       0x3fdf6297cff75caf -0x402e38c4c651973e)
      (0x3fc87de2a6aea964 -0x40226f9430cd72b9 0x3fdd906bcf328d44
       -0x4037821d5951569b -0x4037821d5951568f 0x3fdd906bcf328d4c
       -0x40226f9430cd72b7 0x3fc87de2a6aea924)
      (0x3fb8f8b83c69a60d -0x402e38c4c6519738 0x3fda9b66290ea1a5
       -0x40209d683008a34e 0x3fdf6297cff75cb0 -0x40256499d6f15e68
       0x3fd1c73b39ae68dc -0x40470747c3965a68))) |}]
;;

let%expect_test "inverse" =
  let open Hardcaml_jpeg_model.Dct in
  let inverse = Floating_point.Eight_point.inverse_transform_matrix in
  print_s [%message (inverse : float Matrix8x8.t)];
  let inverse = Matrix8x8.map inverse ~f:Int64.bits_of_float in
  print_s [%message (inverse : Int64.Hex.t Matrix8x8.t)];
  [%expect
    {|
    (inverse
     ((0.35355339059327373 0.49039264020161522 0.46193976625564337
       0.41573480615127262 0.35355339059327379 0.27778511650980114
       0.19134171618254492 0.097545161008064166)
      (0.35355339059327373 0.41573480615127262 0.19134171618254492
       -0.0975451610080641 -0.35355339059327373 -0.49039264020161522
       -0.46193976625564342 -0.27778511650980109)
      (0.35355339059327373 0.27778511650980114 -0.19134171618254486
       -0.49039264020161522 -0.35355339059327384 0.097545161008064152
       0.46193976625564326 0.41573480615127273)
      (0.35355339059327373 0.097545161008064166 -0.46193976625564337
       -0.27778511650980109 0.35355339059327368 0.41573480615127273
       -0.19134171618254495 -0.49039264020161533)
      (0.35355339059327373 -0.0975451610080641 -0.46193976625564342
       0.27778511650980092 0.35355339059327384 -0.41573480615127256
       -0.19134171618254528 0.49039264020161522)
      (0.35355339059327373 -0.277785116509801 -0.19134171618254517
       0.49039264020161533 -0.35355339059327334 -0.097545161008064887
       0.4619397662556437 -0.415734806151272)
      (0.35355339059327373 -0.41573480615127267 0.191341716182545
       0.097545161008063958 -0.35355339059327356 0.49039264020161516
       -0.46193976625564354 0.2777851165098022)
      (0.35355339059327373 -0.49039264020161522 0.46193976625564326
       -0.41573480615127212 0.35355339059327329 -0.27778511650980076
       0.19134171618254314 -0.097545161008062542)))
    (inverse
     ((0x3fd6a09e667f3bcc 0x3fdf6297cff75cb0 0x3fdd906bcf328d46
       0x3fda9b66290ea1a3 0x3fd6a09e667f3bcd 0x3fd1c73b39ae68c9
       0x3fc87de2a6aea964 0x3fb8f8b83c69a60d)
      (0x3fd6a09e667f3bcc 0x3fda9b66290ea1a3 0x3fc87de2a6aea964
       -0x40470747c39659f8 -0x40295f619980c434 -0x40209d683008a350
       -0x40226f9430cd72b9 -0x402e38c4c6519738)
      (0x3fd6a09e667f3bcc 0x3fd1c73b39ae68c9 -0x4037821d5951569e
       -0x40209d683008a350 -0x40295f619980c432 0x3fb8f8b83c69a60c
       0x3fdd906bcf328d44 0x3fda9b66290ea1a5)
      (0x3fd6a09e667f3bcc 0x3fb8f8b83c69a60d -0x40226f9430cd72ba
       -0x402e38c4c6519738 0x3fd6a09e667f3bcb 0x3fda9b66290ea1a5
       -0x4037821d5951569b -0x40209d683008a34e)
      (0x3fd6a09e667f3bcc -0x40470747c39659f8 -0x40226f9430cd72b9
       0x3fd1c73b39ae68c5 0x3fd6a09e667f3bce -0x40256499d6f15e5e
       -0x4037821d5951568f 0x3fdf6297cff75cb0)
      (0x3fd6a09e667f3bcc -0x402e38c4c651973a -0x4037821d59515693
       0x3fdf6297cff75cb2 -0x40295f619980c43b -0x40470747c39659bf
       0x3fdd906bcf328d4c -0x40256499d6f15e68)
      (0x3fd6a09e667f3bcc -0x40256499d6f15e5c 0x3fc87de2a6aea967
       0x3fb8f8b83c69a5fe -0x40295f619980c437 0x3fdf6297cff75caf
       -0x40226f9430cd72b7 0x3fd1c73b39ae68dc)
      (0x3fd6a09e667f3bcc -0x40209d683008a350 0x3fdd906bcf328d44
       -0x40256499d6f15e66 0x3fd6a09e667f3bc4 -0x402e38c4c651973e
       0x3fc87de2a6aea924 -0x40470747c3965a68))) |}]
;;