module type Buffer = sig
  type t [@@deriving sexp_of]

  val length : t -> int
  val get : t -> int -> char
end

module type S = sig
  type t [@@deriving sexp_of]
  type buffer

  val create : buffer -> t
  val get_buffer : t -> buffer

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

(** Bitstream reader functions. *)
module type Bitstream_reader = sig
  module type S = S
  module type Buffer = Buffer

  module Make (Buffer : Buffer) : S with type buffer = Buffer.t
  module From_string : S with type buffer = String.t
end
