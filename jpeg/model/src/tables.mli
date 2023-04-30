open Base

(** {2 Default luma and chroma tables.} *)

type 'a coef =
  { length : int
  ; bits : int
  ; data : 'a
  }
[@@deriving sexp_of]

type dc = int [@@deriving sexp_of]
type dc_coef = dc coef [@@deriving sexp_of]

val dc_luma : dc_coef list
val dc_chroma : dc_coef list

type ac =
  { run : int
  ; size : int
  }
[@@deriving sexp_of]

type ac_coef = ac coef [@@deriving sexp_of]

val ac_luma : ac coef list
val ac_chroma : ac coef list

(** Build a lookup table from a list of coefficients.

    Peek [max_bits] from the bitstream, and use it to index the [lut].  Advance by
    [code.length].

    {[
        let index = Bits.show bits (Lut.max_bits table) in
        let code = (Lut.lut table).(index) in
        Bits.advance bits;
    ]}
*)
module Lut : sig
  type 'a code =
    { length : int
    ; data : 'a
    }

  type 'a t

  val create : 'a coef list -> 'a t
  val lut : 'a t -> 'a code option array
  val max_bits : 'a t -> int
end

(*** Tables indexed by run and size used for encoding. *)
module Encoder : sig
  (** DC coefs sorted by size *)
  val dc_table : dc coef list -> dc coef array

  (*** AC coefs sorted by run and size *)
  val ac_table : ac coef list -> ac coef array array
end
