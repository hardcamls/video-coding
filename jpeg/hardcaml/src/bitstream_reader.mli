(** Bitstream decoder.

    Shows the next 16 bits of the bitstream.  0-16 bits may be removed from the
    bitstream per cycle.  Output data is not always available.
*)

open! Base
open! Hardcaml

module I : sig
  type 'a t =
    { clocking : 'a Clocking.t
    ; header_or_entropy_mode : 'a
    ; read_header_byte : 'a
    ; read_entropy_bits : 'a
    ; jpeg_in : 'a
    ; jpeg_valid : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module O : sig
  type 'a t =
    { bits : 'a
    ; bits_valid : 'a
    ; jpeg_ready : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

val create : Scope.t -> Interface.Create_fn(I)(O).t
val hierarchical : Scope.t -> Interface.Create_fn(I)(O).t
val create_new : Scope.t -> Interface.Create_fn(I)(O).t
