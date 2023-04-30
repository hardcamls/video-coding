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

let size value = if value = 0 then 0 else Int.floor_log2 (Int.abs value) + 1

let encode_bits
    (block : Block.t)
    ~(dc_table : Tables.dc_coef array)
    ~(ac_table : Tables.ac_coef array array)
  =
  let rec ac (c : Rle.t list) =
    match c with
    | [] -> []
    | [ { run = _; value = 0 } ] ->
      (* end of block *) [ { (ac_table.(0).(0)) with data = `eob } ]
    | { run; value } :: tl ->
      let rec runs run =
        if run >= 16
        then { (ac_table.(15).(0)) with data = `ac (15, 0) } :: runs (run - 16)
        else [ { (ac_table.(run).(size value)) with data = `ac (run, value) } ]
      in
      runs run @ ac tl
  in
  match block.rle with
  | { run = 0; value } :: tl -> { (dc_table.(size value)) with data = `dc value } :: ac tl
  | _ -> failwith "no or invalid dc coef?"
;;
