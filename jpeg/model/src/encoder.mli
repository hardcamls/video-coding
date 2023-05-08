(** JPEG Encoder model. *)

open Base
open Hardcaml_video_common

(** A run length coded value from a quantized block. *)
module Rle : sig
  type t =
    { run : int
    ; value : int
    }
  [@@deriving sexp_of]
end

(** Encoded state of a single block. 
    
    Can optionally perform the decoder procedure starting from the quantized coefficients and compute the reconstruction error of each block.
*)
module Block : sig
  module Decoded : sig
    type t =
      { dequant : int array
      ; idct : int array
      ; recon : int array
      ; error : int array
      }
    [@@deriving sexp_of]
  end

  type t =
    { mutable x_pos : int
    ; mutable y_pos : int
    ; input_pixels : int array
    ; fdct : int array
    ; quant : int array
    ; mutable dc_pred : int
    ; mutable rle : Rle.t list
    ; decoded : Decoded.t option
    }
  [@@deriving sexp_of]

  val create : bool -> t
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

module Identified : sig
  type identifier = int

  type 'a t =
    { identifier : identifier
    ; data : 'a
    }
end

module Parameters : sig
  type scan_component =
    { quant_table : Identified.identifier
    ; dc_huffman_table : Identified.identifier
    ; ac_huffman_table : Identified.identifier
    ; component : Identified.identifier
    ; horizontal_sampling_factor : int
    ; vertical_sampling_factor : int
    }

  type t =
    { width : int
    ; height : int
    ; quant_tables : int array Identified.t array
    ; dc_huffman_tables : Tables.Specification.t Identified.t array
    ; ac_huffman_tables : Tables.Specification.t Identified.t array
    ; scan_components : scan_component array
    }

  val c420 : width:int -> height:int -> quality:int -> t
  val c422 : width:int -> height:int -> quality:int -> t
  val c444 : width:int -> height:int -> quality:int -> t
  val monochrome : width:int -> height:int -> quality:int -> t
end

(** Core JPEG encoder.

    1. create
    2. write_headers
    3. blit in image data
    4. Iter encode_seq 
    5. complete
*)

type t

val create
  :  ?compute_reconstruction_error:bool
  -> params:Parameters.t
  -> writer:Bitstream_writer.t
  -> unit
  -> t

val write_headers : params:Parameters.t -> writer:Bitstream_writer.t -> unit
val get_plane : t -> int -> Plane.t
val encode_seq : t -> Block.t Sequence.t
val complete_and_write_eoi : t -> unit

(** {2 Frame encoding} *)

val encode_420 : frame:Frame.t -> quality:int -> writer:Bitstream_writer.t -> unit
val encode_422 : frame:Frame.t -> quality:int -> writer:Bitstream_writer.t -> unit
val encode_444 : frame:Frame.t -> quality:int -> writer:Bitstream_writer.t -> unit
val encode_monochrome : frame:Plane.t -> quality:int -> writer:Bitstream_writer.t -> unit
