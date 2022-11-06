(** Bitstream reader functions. *)

open! Base

module type Buffer = sig
  type t [@@deriving sexp_of]

  val length : t -> int
  val get : t -> int -> char
end

module Make (Buffer : Buffer) : sig
  type t [@@deriving sexp_of]

  val create : Buffer.t -> t

  (** show the next n bits of the bitstream *)
  val show : t -> int -> int

  (** advance the bit pointer by n bits *)
  val advance : t -> int -> unit

  val get_byte : t -> int -> char

  (** show the next n bits of the bitstream and advanced the bit pointer *)
  val get : t -> int -> int

  val bit_pos : t -> int
  val bits_left : t -> int
  val align_to_byte : t -> unit
end
