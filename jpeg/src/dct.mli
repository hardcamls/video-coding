(** IDCT implementation *)

open! Base
open Hardcaml

module I : sig
  type 'a t =
    { clocking : 'a Clocking.t
    ; start : 'a
    ; coef : 'a
    ; transpose_coef_in : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module O : sig
  type 'a t =
    { pixel : 'a
    ; pixel_address : 'a
    ; pixel_write : 'a
    ; transpose_coef_out : 'a
    ; transpose_write_address : 'a
    ; transpose_write : 'a
    ; coef_address : 'a
    ; coef_read : 'a
    ; transpose_read_address : 'a
    ; transpose_read : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

val create : Scope.t -> Signal.t Interface.Create_fn(I)(O).t
