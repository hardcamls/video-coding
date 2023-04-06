open! Base
open Hardcaml

module Core : sig
  module I : sig
    type 'a t =
      { clocking : 'a Clocking.t
      ; start : 'a
      ; markers : 'a Vld.Core.All_markers.t
      ; bits : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { pixel : 'a
      ; read_bits : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  val create : Scope.t -> Interface.Create_fn(I)(O).t
  val hierarchical : Scope.t -> Interface.Create_fn(I)(O).t
end
