open! Base
open! Hardcaml

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

module All_markers : sig
  type 'a t =
    { sof : 'a Markers.Sof.Fields.t
    ; sos : 'a Markers.Sos.Fields.t
    ; dqt : 'a Markers.Dqt.Fields.t
    ; dht : 'a Markers.Dht.Fields.t
    }
  [@@deriving sexp_of, hardcaml]
end

module O : sig
  type 'a t =
    { read_bits : 'a
    ; markers : 'a All_markers.t
    ; error : 'a Error.t
    ; done_ : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

val create : Scope.t -> Interface.Create_fn(I)(O).t
val hierarchical : Scope.t -> Interface.Create_fn(I)(O).t
