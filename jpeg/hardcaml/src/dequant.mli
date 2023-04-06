open! Base
open Hardcaml

module I : sig
  type 'a t =
    { clocking : 'a Clocking.t
    ; coefs_in : 'a Codeblock_decoder.Idct_coefs.t
    ; dqt : 'a Markers.Dqt.Fields.t
    ; table_select : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module O : sig
  type 'a t = { coefs_out : 'a Codeblock_decoder.Idct_coefs.t }
  [@@deriving sexp_of, hardcaml]
end

val create : Scope.t -> Interface.Create_fn(I)(O).t
val hierarchical : Scope.t -> Interface.Create_fn(I)(O).t
