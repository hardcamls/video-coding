open! Base
open! Hardcaml

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

  module Fields : sig
    type 'a t =
      { fields : 'a Header.Fields.t
      ; element : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  include Fields_decoder.M(Fields).S
end
