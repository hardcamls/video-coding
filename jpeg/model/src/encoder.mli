(** JPEG Encoder model. *)

open Base

(** A run length coded value from a quantized block. *)
module Rle : sig
  type t =
    { run : int
    ; value : int
    }
  [@@deriving sexp_of]
end

(** Encoded state of a single block. *)
module Block : sig
  type t =
    { input_pixels : int array
    ; fdct : int array
    ; quant : int array
    ; mutable dc_pred : int
    ; mutable rle : Rle.t list
    ; (* XX For debugging *)
      dequant : int array
    ; idct : int array
    ; recon : int array
    ; error : int array
    }
  [@@deriving sexp_of]

  val create : unit -> t
end

(** {2 Various functions which comprise block encoding.}*)

(** Read 8x8 block from input frame and level shift (subtract 128). *)
val level_shifted_input_block : Plane.t -> Block.t -> x_pos:int -> y_pos:int -> unit

(** Forward DCT transform. *)
val fdct : Block.t -> unit

(** Quantization and zigzag*)
val quant : Block.t -> table:int array -> unit

(** Convert quantized values into run length code. *)
val rle : Block.t -> unit

(** Size of a coded coefficient - roughly the number of bits required to represent it.  *)
val size : int -> int

(** Magnitude code used to code a coefficient of given size. *)
val magnitude : size:int -> int -> int

(** Huffman encode a block. *)
val write_bits
  :  Block.t
  -> writer:Bitstream_writer.t
  -> dc_table:Tables.dc_coef array
  -> ac_table:Tables.ac_coef array array
  -> unit

(** Write JPEG headers. *)
val write_headers
  :  Bitstream_writer.t
  -> width:int
  -> height:int
  -> dc_luma:Tables.Specification.t
  -> ac_luma:Tables.Specification.t
  -> dc_chroma:Tables.Specification.t
  -> ac_chroma:Tables.Specification.t
  -> qnt_luma:int array
  -> qnt_chroma:int array
  -> unit

(** {2 Frame encoding}*)

(** Encode a 4:2:0 input [frame]. 

    [quality] ranges from 1(low) to 100(high).  Around 75 should provide decent images.

    The coded frame is written to [writer].
*)
val encode_420 : frame:Frame.t -> quality:int -> writer:Bitstream_writer.t -> unit
