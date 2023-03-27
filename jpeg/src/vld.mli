open! Base
open! Hardcaml

module Core : sig
  module I : sig
    type 'a t =
      { clocking : 'a Clocking.t
      ; start : 'a
      ; bits : 'a
      ; bits_valid : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Error : sig
    module T : sig
      type t =
        | Ok
        | Decode_error
      [@@deriving sexp_of, compare, enumerate]
    end

    include Enum.S_enum with module Cases := T

    val to_string : T.t -> string
  end

  module O : sig
    type 'a t =
      { coef : 'a
      ; run : 'a
      ; write : 'a
      ; read_bits : 'a
      ; error : 'a Error.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  val create : Scope.t -> Interface.Create_fn(I)(O).t
  val hierarchical : Scope.t -> Interface.Create_fn(I)(O).t
end

module With_reader : sig
  module I : sig
    type 'a t =
      { clocking : 'a Clocking.t
      ; start : 'a
      ; bits_in : 'a
      ; bits_in_available : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { coef : 'a
      ; run : 'a
      ; write : 'a
      ; error : 'a Core.Error.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  val create : Scope.t -> Interface.Create_fn(I)(O).t
  val hierarchical : Scope.t -> Interface.Create_fn(I)(O).t
end
