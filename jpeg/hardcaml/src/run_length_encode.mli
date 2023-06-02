open! Base
open Hardcaml

module Rle : sig
  type 'a t =
    { run : 'a
    ; coef : 'a
    ; last : 'a
    ; dc : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module Rle_out : With_valid.Wrap.M(Rle).S

module I : sig
  type 'a t =
    { clocking : 'a Clocking.t
    ; start : 'a
    ; coef : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module O : sig
  type 'a t =
    { rle_out : 'a Rle_out.t
    ; quant_address : 'a
    ; quant_read : 'a
    ; done_ : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

val create : Scope.t -> Signal.t I.t -> Signal.t O.t
val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
