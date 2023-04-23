open Base
open Hardcaml

module I : sig
  type 'a t =
    { clocking : 'a Clocking.t
    ; start : 'a
    ; jpeg : 'a
    ; jpeg_valid : 'a
    ; bits_ready : 'a
    }
  [@@deriving sexp_of, hardcaml]
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
    { jpeg_ready : 'a
    ; done_ : 'a
    ; bits : 'a
    ; bits_valid : 'a
    ; markers : 'a All_markers.t
    }
  [@@deriving sexp_of, hardcaml]
end

module State : sig
  type t

  val names : string list
end

val create : Scope.t -> Interface.Create_fn(I)(O).t
