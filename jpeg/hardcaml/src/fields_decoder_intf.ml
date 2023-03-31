open! Base
open! Hardcaml

module M (Fields : Interface.S) = struct
  module type S = sig
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

    val create : Scope.t -> Interface.Create_fn(I)(O).t
    val hierarchical : ?name:string -> Scope.t -> Interface.Create_fn(I)(O).t
  end
end

module Ports (Fields : Interface.S) = struct
  module I = struct
    type 'a t =
      { clocking : 'a Clocking.t
      ; start : 'a
      ; bits : 'a [@bits 16]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { read_bits : 'a [@bits 5]
      ; fields : 'a Fields.t
      ; done_ : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end
end

module type Fields_decoder = sig
  module M = M
  module Ports = Ports

  module Make (Fields : Interface.S) : sig
    module State : sig
      type t [@@deriving sexp_of, compare, enumerate]
    end

    include M(Fields).S
  end
end
