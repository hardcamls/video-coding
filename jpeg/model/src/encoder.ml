open! Base

(* 
   - Level shift
   - FDCT
   - Quantize
   - DC-pred
   - Zigzag
   - Run-length encode
   - Huffman encode
   
*)

module Rle = struct
  type t =
    { run : int
    ; value : int
    }
  [@@deriving sexp_of]
end

module Block = struct
  type t =
    { level_shifted_pixels : int array
    ; fdct : int array
    ; quant : int array
    ; mutable dc_pred : int
    ; mutable rle : Rle.t list
    }
  [@@deriving sexp_of]

  let create () =
    { level_shifted_pixels = Array.create ~len:64 0
    ; fdct = Array.create ~len:64 0
    ; quant = Array.create ~len:64 0
    ; dc_pred = 0
    ; rle = []
    }
  ;;
end

let level_shifted_input_block plane (block : Block.t) ~x_pos ~y_pos =
  for y = 0 to 7 do
    for x = 0 to 7 do
      let k = (y * 8) + x in
      let p = Char.to_int Plane.(plane.![x + x_pos, y + y_pos]) - 128 in
      block.level_shifted_pixels.(k) <- p;
      block.fdct.(k) <- p
    done
  done
;;

let fdct (block : Block.t) = Dct.Chen.forward_8x8 block.fdct

let quant (block : Block.t) ~table =
  for i = 0 to 63 do
    block.quant.(i) <- (block.fdct.(i) + (table.(i) / 2)) / table.(i)
  done
;;

let rle (block : Block.t) =
  let quant = block.quant in
  let rec rle run pos =
    if pos = 63
    then [ { Rle.run; value = quant.(Zigzag.forward.(pos)) } ]
    else (
      let value = quant.(Zigzag.forward.(pos)) in
      if value <> 0 then { run; value } :: rle 0 (pos + 1) else rle (run + 1) (pos + 1))
  in
  let dc = quant.(0) in
  block.rle <- { run = 0; value = dc - block.dc_pred } :: rle 0 1;
  block.dc_pred <- dc
;;
