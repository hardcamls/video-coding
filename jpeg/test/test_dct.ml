(* Test forward and inverse DCTs. *)
open Core

(* Integer forward and inverse DCT implementation used in software model.

   Check roundtrip accuracy. *)

let test_chen_transform () =
  let input = Array.init 64 ~f:(fun _ -> Random.int 256) in
  let fdct = Array.copy input in
  Hardcaml_jpeg_model.Dct.forward_dct_8x8 fdct;
  let idct = Array.copy fdct in
  Hardcaml_jpeg_model.Dct.inverse_dct_8x8 idct;
  Array.iter2_exn input idct ~f:(fun a b ->
      let b = Float.of_int b /. 4. in
      let b = Float.round_nearest b |> Float.to_int in
      (* a tolerance of 2 seems to work. Thats fine - the accuracy of the IDCT
         is more important, and I think is better than this. *)
      if Int.abs (a - b) > 2 then raise_s [%message (a : int) (b : int)])
;;

let%expect_test "compare forward and inverse dcts" =
  for _ = 1 to 100 do
    test_chen_transform ()
  done;
  [%expect {||}]
;;

(* Reference inverse DCT implementations*)

let even_idct_4pt_coefs = Hardcaml_jpeg.Dct.even_idct_4pt_coefs
let odd_idct_4pt_coefs = Hardcaml_jpeg.Dct.odd_idct_4pt_coefs

let%expect_test "idct coefficients" =
  print_s
    [%message
      (even_idct_4pt_coefs : float array array) (odd_idct_4pt_coefs : float array array)];
  [%expect
    {|
    ((even_idct_4pt_coefs
      ((0.35355339059327373 0.46193976625564337 0.35355339059327379
        0.19134171618254492)
       (0.35355339059327373 0.19134171618254492 -0.35355339059327373
        -0.46193976625564342)
       (0.35355339059327373 -0.19134171618254486 -0.35355339059327384
        0.46193976625564326)
       (0.35355339059327373 -0.46193976625564337 0.35355339059327368
        -0.19134171618254495)))
     (odd_idct_4pt_coefs
      ((0.49039264020161522 0.41573480615127262 0.27778511650980114
        0.097545161008064166)
       (0.41573480615127262 -0.0975451610080641 -0.49039264020161522
        -0.27778511650980109)
       (0.27778511650980114 -0.49039264020161522 0.097545161008064152
        0.41573480615127273)
       (0.097545161008064166 -0.27778511650980109 0.41573480615127273
        -0.49039264020161533)))) |}]
;;

let idct_8pt_from_4pt b =
  let mul4 a b =
    Float.((a.(0) * b.(0)) + (a.(1) * b.(1)) + (a.(2) * b.(2)) + (a.(3) * b.(3)))
  in
  (* 4pt idct of even coefs *)
  let even = Array.init 4 ~f:(fun i -> b.(i * 2)) in
  let even = Array.init 4 ~f:(fun i -> mul4 even even_idct_4pt_coefs.(i)) in
  (* 4pt idct of even coefs *)
  let odd = Array.init 4 ~f:(fun i -> b.((i * 2) + 1)) in
  let odd = Array.init 4 ~f:(fun i -> mul4 odd odd_idct_4pt_coefs.(i)) in
  (* butterfly step to combine to 8pt idct *)
  Array.init 8 ~f:(fun i ->
      if i < 4
      then Float.( + ) even.(i) odd.(i)
      else Float.( - ) even.(3 - (i - 4)) odd.(3 - (i - 4)))
;;

let idct_8pt b =
  let x = Array.create ~len:8 0.0 in
  let scl = Float.(sqrt (2. / 8.)) in
  for k = 0 to 7 do
    x.(k) <- Float.(sqrt 2. * b.(0) / 2.);
    for n = 1 to 7 do
      x.(k) <- Float.(x.(k) + (b.(n) * cos (pi * of_int n * (of_int k + 0.5) / 8.)))
    done;
    x.(k) <- Float.(scl * x.(k))
  done;
  x
;;

let%expect_test "test reference idcts" =
  let i = Array.init 8 ~f:(fun _ -> Random.float 1000. -. 500.) in
  let o1 = idct_8pt_from_4pt i in
  let o2 = idct_8pt i in
  print_s [%message (o1 : float array) (o2 : float array)];
  [%expect
    {|
    ((o1
      (82.154934359857748 528.35926070955452 -237.01312484581763
       139.43852814753859 -59.17923277584601 -150.08985209325476
       -429.75983385459324 -54.065750159214296))
     (o2
      (82.154934359857762 528.35926070955452 -237.01312484581769
       139.43852814753859 -59.179232775845861 -150.08985209325485
       -429.75983385459324 -54.065750159214424))) |}]
;;

(* Reference forward DCT implementations*)

let even_dct_4pt_coefs =
  Array.init 4 ~f:(fun i ->
      Array.init 4 ~f:(fun j ->
          if i = 0
          then Float.(0.5 / sqrt 2.0)
          else (
            let i = Float.of_int i in
            let j = Float.of_int j in
            Float.(0.5 * cos (((2.0 * j) + 1.0) * i * 2.0 * pi / 16.0)))))
;;

let odd_dct_4pt_coefs =
  Array.init 4 ~f:(fun i ->
      Array.init 4 ~f:(fun j ->
          let i = Float.of_int i in
          let j = Float.of_int j in
          Float.(0.5 * cos (((2.0 * j) + 1.0) * ((i * 2.0) + 1.0) * pi / 16.0))))
;;

let fdct_8pt_from_4pt b =
  let mul4 a b =
    Float.((a.(0) * b.(0)) + (a.(1) * b.(1)) + (a.(2) * b.(2)) + (a.(3) * b.(3)))
  in
  let u = Array.init 4 ~f:(fun i -> Float.( + ) b.(i) b.(7 - i)) in
  let v = Array.init 4 ~f:(fun i -> Float.( - ) b.(i) b.(7 - i)) in
  Array.init 8 ~f:(fun i ->
      if i % 2 = 0
      then mul4 even_dct_4pt_coefs.(i / 2) u
      else mul4 odd_dct_4pt_coefs.(i / 2) v)
;;

let fdct_8pt din =
  let out = Array.create ~len:8 0.0 in
  for v = 0 to 7 do
    let fv = Float.of_int v in
    for i = 0 to 7 do
      let fi = Float.of_int i in
      let scl =
        if v <> 0 then Float.(sqrt (1.0 / 4.0)) else Float.(sqrt (1.0 / 4.0) * sqrt 0.5)
      in
      let th = Float.(((2.0 * fi) + 1.0) * fv * pi / 16.0) in
      let coeff = Float.(cos th * scl) in
      out.(v) <- Float.(out.(v) + (coeff * din.(i)))
    done
  done;
  out
;;

let%expect_test "test reference dcts" =
  let i = Array.init 8 ~f:(fun _ -> Random.float 1000. -. 500.) in
  let o1 = fdct_8pt_from_4pt i in
  let o2 = fdct_8pt i in
  print_s [%message (o1 : float array) (o2 : float array)];
  [%expect
    {|
    ((o1
      (-149.28500626602016 547.1083997783436 -151.36455670680471
       -90.191485021748576 -104.6809185826398 -288.26284304019043
       -341.46256474249572 108.93777954358424))
     (o2
      (-149.28500626602016 547.1083997783436 -151.36455670680459
       -90.191485021748719 -104.68091858263976 -288.26284304019055
       -341.46256474249589 108.9377795435843))) |}]
;;

open Hardcaml
open Hardcaml_waveterm
module Dct = Hardcaml_jpeg.Dct.Single_multiplier

let%expect_test "test loops" =
  let module Sim = Cyclesim.With_interface (Dct.I) (Dct.O) in
  let sim = Sim.create Dct.create in
  let waves, sim = Waveform.create sim in
  for _ = 0 to 10 do
    Cyclesim.cycle sim
  done;
  Waveform.print waves;
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clock          ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌──│
    │               ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘  │
    │clear          ││                                                   │
    │               ││───────────────────────────────────────────────────│
    │               ││───────────────────────────────────────────────────│
    │read_address   ││ 00                                                │
    │               ││───────────────────────────────────────────────────│
    │               ││───────────────────────────────────────────────────│
    │read_enable    ││ 0                                                 │
    │               ││───────────────────────────────────────────────────│
    │               ││────────────────┬───────┬───────┬───────┬───────┬──│
    │write_address  ││ 00             │01     │02     │03     │04     │05│
    │               ││────────────────┴───────┴───────┴───────┴───────┴──│
    │               ││───────────────────────────────────────────────────│
    │write_coef     ││ 00                                                │
    │               ││───────────────────────────────────────────────────│
    │               ││───────────────────────────────────────────────────│
    │write_enable   ││ 0                                                 │
    └───────────────┘└───────────────────────────────────────────────────┘ |}]
;;
