open Base
open Hardcaml

val dct_coef_bits : int
val quant_coef_bits : int
val log_num_quant_tables : int
val num_quant_tables : int
val pipeline_depth : int
val one_over_quant_coef : int -> int

module Quant_write : sig
  type 'a t =
    { quant : 'a
    ; write : 'a
    ; address : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module I : sig
  type 'a t =
    { clocking : 'a Clocking.t
    ; enable : 'a
    ; table_select : 'a
    ; dct_coef : 'a
    ; dct_coef_write : 'a
    ; dct_coef_address : 'a
    ; quant : 'a Quant_write.t
    }
  [@@deriving sexp_of, hardcaml]
end

module O : sig
  type 'a t =
    { quant_coef : 'a
    ; quant_coef_write : 'a
    ; quant_coef_address : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

val multiply : (module Comb.S with type t = 'a) -> 'a -> 'a -> 'a
val round : (module Comb.S with type t = 'a) -> 'a -> 'a
val create : Scope.t -> Signal.t I.t -> Signal.t O.t
val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
