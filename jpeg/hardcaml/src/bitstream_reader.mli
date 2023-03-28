(** Bitstream decoder.

    Shows the next 16 bits of the bitstream.  0-16 bits may be removed from the
    bitstream per cycle.  Output data is not always available.
*)

open! Base
open! Hardcaml

module I : sig
  type 'a t =
    { clocking : 'a Clocking.t
    ; advance_bits : 'a
    ; bits_in : 'a
    ; bits_in_available : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module O : sig
  type 'a t =
    { bits : 'a
    ; bits_out_available : 'a
    ; read_bits_in : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

val create : Scope.t -> Interface.Create_fn(I)(O).t
val hierarchical : Scope.t -> Interface.Create_fn(I)(O).t
