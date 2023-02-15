open Base

module Chen = struct
  let w1 = 2841
  let w2 = 2676
  let w3 = 2408
  let w5 = 1609
  let w6 = 1108
  let w7 = 565

  let idct_row block row =
    let off = row * 8 in
    let x0 = (block.(off + 0) lsl 11) + 128 in
    let x1 = block.(off + 4) lsl 11 in
    let x2 = block.(off + 6) in
    let x3 = block.(off + 2) in
    let x4 = block.(off + 1) in
    let x5 = block.(off + 7) in
    let x6 = block.(off + 5) in
    let x7 = block.(off + 3) in
    (* first stage *)
    let x8 = w7 * (x4 + x5) in
    let x4 = x8 + ((w1 - w7) * x4) in
    let x5 = x8 - ((w1 + w7) * x5) in
    let x8 = w3 * (x6 + x7) in
    let x6 = x8 - ((w3 - w5) * x6) in
    let x7 = x8 - ((w3 + w5) * x7) in
    (* second stage *)
    let x8 = x0 + x1 in
    let x0 = x0 - x1 in
    let x1 = w6 * (x3 + x2) in
    let x2 = x1 - ((w2 + w6) * x2) in
    let x3 = x1 + ((w2 - w6) * x3) in
    let x1 = x4 + x6 in
    let x4 = x4 - x6 in
    let x6 = x5 + x7 in
    let x5 = x5 - x7 in
    (* third stage *)
    let x7 = x8 + x3 in
    let x8 = x8 - x3 in
    let x3 = x0 + x2 in
    let x0 = x0 - x2 in
    let x2 = ((181 * (x4 + x5)) + 128) asr 8 in
    let x4 = ((181 * (x4 - x5)) + 128) asr 8 in
    (* fourth stage *)
    block.(off + 0) <- (x7 + x1) asr 8;
    block.(off + 1) <- (x3 + x2) asr 8;
    block.(off + 2) <- (x0 + x4) asr 8;
    block.(off + 3) <- (x8 + x6) asr 8;
    block.(off + 4) <- (x8 - x6) asr 8;
    block.(off + 5) <- (x0 - x4) asr 8;
    block.(off + 6) <- (x3 - x2) asr 8;
    block.(off + 7) <- (x7 - x1) asr 8
  ;;

  let idct_col block col =
    let x0 = (block.(col + (8 * 0)) lsl 8) + 8192 in
    let x1 = block.(col + (8 * 4)) lsl 8 in
    let x2 = block.(col + (8 * 6)) in
    let x3 = block.(col + (8 * 2)) in
    let x4 = block.(col + (8 * 1)) in
    let x5 = block.(col + (8 * 7)) in
    let x6 = block.(col + (8 * 5)) in
    let x7 = block.(col + (8 * 3)) in
    (* first stage *)
    let x8 = (w7 * (x4 + x5)) + 4 in
    let x4 = (x8 + ((w1 - w7) * x4)) asr 3 in
    let x5 = (x8 - ((w1 + w7) * x5)) asr 3 in
    let x8 = (w3 * (x6 + x7)) + 4 in
    let x6 = (x8 - ((w3 - w5) * x6)) asr 3 in
    let x7 = (x8 - ((w3 + w5) * x7)) asr 3 in
    (* second stage *)
    let x8 = x0 + x1 in
    let x0 = x0 - x1 in
    let x1 = (w6 * (x3 + x2)) + 4 in
    let x2 = (x1 - ((w2 + w6) * x2)) asr 3 in
    let x3 = (x1 + ((w2 - w6) * x3)) asr 3 in
    let x1 = x4 + x6 in
    let x4 = x4 - x6 in
    let x6 = x5 + x7 in
    let x5 = x5 - x7 in
    (* third stage *)
    let x7 = x8 + x3 in
    let x8 = x8 - x3 in
    let x3 = x0 + x2 in
    let x0 = x0 - x2 in
    let x2 = ((181 * (x4 + x5)) + 128) asr 8 in
    let x4 = ((181 * (x4 - x5)) + 128) asr 8 in
    (* fourth stage *)
    block.(col + (8 * 0)) <- (x7 + x1) asr 14;
    block.(col + (8 * 1)) <- (x3 + x2) asr 14;
    block.(col + (8 * 2)) <- (x0 + x4) asr 14;
    block.(col + (8 * 3)) <- (x8 + x6) asr 14;
    block.(col + (8 * 4)) <- (x8 - x6) asr 14;
    block.(col + (8 * 5)) <- (x0 - x4) asr 14;
    block.(col + (8 * 6)) <- (x3 - x2) asr 14;
    block.(col + (8 * 7)) <- (x7 - x1) asr 14
  ;;

  let inverse_8x8 block =
    for i = 0 to 7 do
      idct_row block i
    done;
    for i = 0 to 7 do
      idct_col block i
    done
  ;;

  let c4 f g = (362 * (f + g)) asr 9
  let c62 f g = ((196 * f) + (473 * g)) asr 9
  let c71 f g = ((100 * f) + (502 * g)) asr 9
  let c35 f g = ((426 * f) + (284 * g)) asr 9

  let dct_col block col =
    let p1 = col in
    let p2 = col + 56 in
    let a0 = block.(p1) + block.(p2) in
    let c3 = block.(p1) - block.(p2) in
    let p1 = p1 + 8 in
    let p2 = p2 - 8 in
    let a1 = block.(p1) + block.(p2) in
    let c2 = block.(p1) - block.(p2) in
    let p1 = p1 + 8 in
    let p2 = p2 - 8 in
    let a2 = block.(p1) + block.(p2) in
    let c1 = block.(p1) - block.(p2) in
    let p1 = p1 + 8 in
    let p2 = p2 - 8 in
    let a3 = block.(p1) + block.(p2) in
    let c0 = block.(p1) - block.(p2) in
    let b0 = a0 + a3 in
    let b1 = a1 + a2 in
    let b2 = a1 - a2 in
    let b3 = a0 - a3 in
    block.(0 + col) <- c4 b0 b1;
    block.(32 + col) <- c4 b0 (-b1);
    block.(16 + col) <- c62 b2 b3;
    block.(48 + col) <- c62 b3 (-b2);
    let b0 = c4 c2 (-c1) in
    let b1 = c4 c2 c1 in
    let a0 = c0 + b0 in
    let a1 = c0 - b0 in
    let a2 = c3 - b1 in
    let a3 = c3 + b1 in
    block.(8 + col) <- c71 a0 a3;
    block.(40 + col) <- c35 a1 a2;
    block.(24 + col) <- c35 a2 (-a1);
    block.(56 + col) <- c71 a3 (-a0)
  ;;

  let dct_row block row =
    let p1 = row * 8 in
    let p2 = p1 + 7 in
    let a0 = block.(p1) + block.(p2) in
    let c3 = block.(p1) - block.(p2) in
    let p1 = p1 + 1 in
    let p2 = p2 - 1 in
    let a1 = block.(p1) + block.(p2) in
    let c2 = block.(p1) - block.(p2) in
    let p1 = p1 + 1 in
    let p2 = p2 - 1 in
    let a2 = block.(p1) + block.(p2) in
    let c1 = block.(p1) - block.(p2) in
    let p1 = p1 + 1 in
    let p2 = p2 - 1 in
    let a3 = block.(p1) + block.(p2) in
    let c0 = block.(p1) - block.(p2) in
    let b0 = a0 + a3 in
    let b1 = a1 + a2 in
    let b2 = a1 - a2 in
    let b3 = a0 - a3 in
    let row = row * 8 in
    block.(row + 0) <- c4 b0 b1;
    block.(row + 4) <- c4 b0 (-b1);
    block.(row + 2) <- c62 b2 b3;
    block.(row + 6) <- c62 b3 (-b2);
    let b0 = c4 c2 (-c1) in
    let b1 = c4 c2 c1 in
    let a0 = c0 + b0 in
    let a1 = c0 - b0 in
    let a2 = c3 - b1 in
    let a3 = c3 + b1 in
    block.(row + 1) <- c71 a0 a3;
    block.(row + 5) <- c35 a1 a2;
    block.(row + 3) <- c35 a2 (-a1);
    block.(row + 7) <- c71 a3 (-a0)
  ;;

  let forward_8x8 block =
    for i = 0 to 7 do
      dct_col block i
    done;
    for i = 0 to 7 do
      dct_row block i
    done
  ;;
end

module Matrix8x8 = struct
  type 'a t = 'a array array [@@deriving sexp_of]

  let init f = Array.init 8 ~f:(fun row -> Array.init 8 ~f:(fun col -> f ~row ~col))
  let transpose a = Array.init 8 ~f:(fun i -> Array.init 8 ~f:(fun j -> a.(j).(i)))
  let map a ~f = Array.map a ~f:(Array.map ~f)
  let map2 a b ~f = Array.map2_exn a b ~f:(Array.map2_exn ~f)
  let mapi a ~f = Array.mapi a ~f:(fun row -> Array.mapi ~f:(fun col -> f ~row ~col))
  let iter a ~f = Array.iter a ~f:(Array.iter ~f)
  let iteri a ~f = Array.iteri a ~f:(fun row -> Array.iteri ~f:(fun col -> f ~row ~col))

  let fmul a b =
    Array.init 8 ~f:(fun row ->
        Array.init 8 ~f:(fun col ->
            let sum = ref 0. in
            for k = 0 to 7 do
              sum := !sum +. (a.(row).(k) *. b.(k).(col))
            done;
            !sum))
  ;;

  let imul a b =
    Array.init 8 ~f:(fun row ->
        Array.init 8 ~f:(fun col ->
            let sum = ref 0 in
            for k = 0 to 7 do
              sum := !sum + (a.(row).(k) * b.(k).(col))
            done;
            !sum))
  ;;

  let iclip ~min ~max m =
    map m ~f:(fun x -> if x < min then min else if x > max then max else x)
  ;;
end

module Matrix4x4 = struct
  type 'a t = 'a array array [@@deriving sexp_of]
end

module Floating_point = struct
  open Matrix8x8

  module Eight_point = struct
    let forward_transform_matrix =
      let n = 8. in
      Array.init 8 ~f:(fun row ->
          Array.init 8 ~f:(fun col ->
              if row = 0
              then 1. /. Float.sqrt n
              else (
                let row = Float.of_int row in
                let col = Float.of_int col in
                Float.sqrt (2. /. n) *. Float.cos (Float.pi /. n *. (col +. 0.5) *. row))))
    ;;

    let inverse_transform_matrix = transpose forward_transform_matrix

    let forward_transform a =
      fmul (fmul forward_transform_matrix a) (transpose forward_transform_matrix)
    ;;

    let inverse_transform a =
      fmul (fmul inverse_transform_matrix a) (transpose inverse_transform_matrix)
    ;;
  end

  module Using_four_point = struct
    let even_fdct_4pt_coefs =
      Array.init 4 ~f:(fun i ->
          Array.init 4 ~f:(fun j ->
              if i = 0
              then Float.(0.5 / sqrt 2.0)
              else (
                let i = Float.of_int i in
                let j = Float.of_int j in
                Float.(0.5 * cos (((2.0 * j) + 1.0) * i * 2.0 * pi / 16.0)))))
    ;;

    let odd_fdct_4pt_coefs =
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
          then mul4 even_fdct_4pt_coefs.(i / 2) u
          else mul4 odd_fdct_4pt_coefs.(i / 2) v)
    ;;

    let forward_transform t =
      Array.map t ~f:fdct_8pt_from_4pt
      |> Matrix8x8.transpose
      |> Array.map ~f:fdct_8pt_from_4pt
      |> Matrix8x8.transpose
    ;;

    let even_idct_4pt_coefs =
      Array.init 4 ~f:(fun i ->
          let f j =
            Float.(
              0.5 * cos (((2. * Float.of_int i) + 1.) * (2. * Float.of_int j) * pi / 16.))
          in
          Float.[| 0.5 / sqrt 2.; f 1; f 2; f 3 |])
    ;;

    let odd_idct_4pt_coefs =
      Array.init 4 ~f:(fun j ->
          Array.init 4 ~f:(fun i ->
              Float.(
                0.5
                * cos
                    (((2. * Float.of_int i) + 1.)
                    * ((2. * Float.of_int j) + 1.)
                    * pi
                    / 16.))))
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

    let inverse_transform t =
      Array.map t ~f:idct_8pt_from_4pt
      |> Matrix8x8.transpose
      |> Array.map ~f:idct_8pt_from_4pt
      |> Matrix8x8.transpose
    ;;
  end
end

module Fixed_point = struct
  let fixed_coefs ~fixed_prec coefs =
    let fixed_scale = Float.(2. ** of_int fixed_prec) in
    Array.map
      coefs
      ~f:(Array.map ~f:(fun c -> Float.(round_nearest (c * fixed_scale) |> to_int)))
  ;;

  (* tie away from 0 *)
  let round x ~fixed_prec =
    let half = 1 lsl (fixed_prec - 1) in
    let floor x = x asr fixed_prec in
    let ceil x = floor (x + ((1 lsl fixed_prec) - 1)) in
    if x < 0 then ceil (x - half) else floor (x + half)
  ;;

  let round_matrix m ~prec =
    if prec = 0
    then m
    else if prec < 0
    then (
      let prec = -prec in
      Matrix8x8.map m ~f:(fun x -> x lsl prec))
    else Matrix8x8.map m ~f:(round ~fixed_prec:prec)
  ;;

  let transform transform_matrix ~rom_prec ~transpose_prec inputs =
    assert (rom_prec >= 0);
    assert (transpose_prec >= 0);
    let coefs = fixed_coefs ~fixed_prec:rom_prec transform_matrix in
    let transpose = Matrix8x8.imul coefs inputs in
    (* scale to new precision *)
    let transpose = round_matrix transpose ~prec:(rom_prec - transpose_prec) in
    let result = Matrix8x8.imul transpose (Matrix8x8.transpose coefs) in
    round_matrix result ~prec:(rom_prec + transpose_prec)
  ;;

  let forward_transform = transform Floating_point.Eight_point.forward_transform_matrix
  let inverse_transform = transform Floating_point.Eight_point.inverse_transform_matrix
end
