open! Base
open! Hardcaml

module I : sig
  type 'a t =
    { clocking : 'a Clocking.t
    ; bits : 'a
    ; num_bits : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module O : sig
  type 'a t =
    { bits_out : 'a [@bits 16]
    ; bits_valid : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module Make (Comb : Comb.S) : sig
  open Comb

  val insert_at_bottom : buffer:t -> data_in:t -> bits:t -> t
end

val create : Scope.t -> Interface.Create_fn(I)(O).t
