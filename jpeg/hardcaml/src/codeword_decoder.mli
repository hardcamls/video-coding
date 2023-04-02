open! Base
open Hardcaml
module Code = Markers.Dht.Code

module I : sig
  type 'a t =
    { clocking : 'a Clocking.t
    ; code_in : 'a Code.t
    ; bits : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module Decoded_code : sig
  type 'a t =
    { data_address : 'a
    ; length : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module O : sig
  type 'a t =
    { matches : 'a
    ; decoded : 'a Decoded_code.t
    }
  [@@deriving sexp_of, hardcaml]
end

val create : Scope.t -> Interface.Create_fn(I)(O).t
val hierarchical : Scope.t -> name:string -> Interface.Create_fn(I)(O).t
