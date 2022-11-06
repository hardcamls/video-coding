(** JPEG decoding model. *)

open! Base
module Bits : module type of Bitstream_reader.Make (String)

module Header : sig
  type t [@@deriving sexp_of]

  val empty : t
  val decode : Bits.t -> t -> t
end

val decode_frame : Bits.t -> Plane.t array
