open! Base
open! Hardcaml

module Coef : sig
  type 'a t =
    { coef : 'a
    ; run : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module I : sig
  type 'a t =
    { clocking : 'a Clocking.t
    ; start : 'a
    ; coef : 'a Coef.t
    ; luma : 'a
    ; bits_writer_ready : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module O : sig
  type 'a t =
    { bits : 'a
    ; write_bits : 'a
    ; coef_address : 'a [@bits 6]
    ; coef_read_enable : 'a
    ; done_ : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module Var = Hardcaml.Always.Variable

val size : (module Comb.S with type t = 'a) -> 'a -> 'a
val mag : (module Comb.S with type t = 'a) -> 'a -> 'a -> 'a
val create : Scope.t -> Interface.Create_fn(I)(O).t
