open Base

(* 8x8 Chen Wang DCT*)

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

let inverse_dct_8x8 block =
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

let forward_dct_8x8 block =
  for i = 0 to 7 do
    dct_col block i
  done;
  for i = 0 to 7 do
    dct_row block i
  done
;;
