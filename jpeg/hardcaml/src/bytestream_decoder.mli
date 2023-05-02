(** Bytestream decoder. 
    
    JPEG input data is accepted 1 byte per cycle.  The headers are decoded then the entropy coded
    segment is output with sutffing bytes removed.

    This block performs top level management control of the decoder process.
*)

open Base
open Hardcaml

module I : sig
  type 'a t =
    { clocking : 'a Clocking.t
    ; start : 'a
    ; jpeg : 'a
    ; jpeg_valid : 'a
    ; bits_ready : 'a
    ; decoder_done : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module O : sig
  type 'a t =
    { jpeg_ready : 'a
    ; done_ : 'a
    ; bits : 'a
    ; bits_valid : 'a
    ; markers : 'a Markers.All.t
    ; decoder_start : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module State : sig
  type t

  val names : string list
end

val create : Scope.t -> Interface.Create_fn(I)(O).t
val hierarchical : Scope.t -> Interface.Create_fn(I)(O).t

module With_fifo16 : sig
  module I : sig
    type 'a t =
      { clocking : 'a Clocking.t
      ; start : 'a
      ; jpeg : 'a
      ; jpeg_valid : 'a
      ; bits_ready : 'a
      ; decoder_done : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { jpeg_ready : 'a
      ; done_ : 'a
      ; bits : 'a
      ; bits_valid : 'a
      ; markers : 'a Markers.All.t
      ; decoder_start : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  val create : capacity_in_bytes:int -> Scope.t -> Interface.Create_fn(I)(O).t
  val hierarchical : capacity_in_bytes:int -> Scope.t -> Interface.Create_fn(I)(O).t
end
