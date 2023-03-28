open! Base
open Hardcaml

module Dqt : sig
  module Header : sig
    module Fields : sig
      type 'a t =
        { length : 'a
        ; element_precision : 'a
        ; table_identifier : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end
  end

  module I : sig
    type 'a t =
      { clocking : 'a Clocking.t
      ; start : 'a
      ; bits : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Fields : sig
    type 'a t =
      { fields : 'a Header.Fields.t
      ; element : 'a
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
  val hierarchical : Scope.t -> Interface.Create_fn(I)(O).t
end
