open Base

module Rle : sig
  type t =
    { run : int
    ; value : int
    }
  [@@deriving sexp_of]
end

module Block : sig
  type t =
    { level_shifted_pixels : int array
    ; fdct : int array
    ; quant : int array
    ; mutable dc_pred : int
    ; mutable rle : Rle.t list
    }
  [@@deriving sexp_of]

  val create : unit -> t
end

val level_shifted_input_block : Plane.t -> Block.t -> x_pos:int -> y_pos:int -> unit
val fdct : Block.t -> unit
val quant : Block.t -> table:int array -> unit
val rle : Block.t -> unit
