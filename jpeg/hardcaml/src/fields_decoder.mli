open Hardcaml

module Make (Fields : Interface.S) : sig
  module I : sig
    type 'a t =
      { clocking : 'a Clocking.t
      ; start : 'a
      ; bits : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { read_bits : 'a
      ; fields : 'a Fields.t
      ; done_ : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module State : sig
    type t [@@deriving sexp_of, compare, enumerate]
  end

  val create : Scope.t -> Interface.Create_fn(I)(O).t
  val hierarchical : name:string -> Scope.t -> Interface.Create_fn(I)(O).t
end
