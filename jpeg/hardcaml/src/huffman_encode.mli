open! Base
open! Hardcaml

module I : sig
  type 'a t =
    { clocking : 'a Clocking.t
    ; start : 'a
    ; rle_in : 'a Run_length_encode.Rle_out.t
    ; luma : 'a
    ; bits_writer_ready : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module O : sig
  type 'a t =
    { bits : 'a
    ; write_bits : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module Var = Hardcaml.Always.Variable

val size : (module Comb.S with type t = 'a) -> 'a -> 'a
val mag : (module Comb.S with type t = 'a) -> 'a -> 'a -> 'a
val create : Scope.t -> Interface.Create_fn(I)(O).t
