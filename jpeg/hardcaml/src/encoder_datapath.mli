open! Base
open! Hardcaml

module Starts : sig
  type 'a t =
    { dct : 'a
    ; vlc : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module Pixels : sig
  type 'a t =
    { data : 'a
    ; write_address : 'a
    ; write_enable : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module I : sig
  type 'a t =
    { clocking : 'a Clocking.t
    ; starts : 'a Starts.t
    ; pixels : 'a Pixels.t
    ; quant_write : 'a Quant.Quant_write.t
    }
  [@@deriving sexp_of, hardcaml]
end

module O : sig
  type 'a t =
    { rle_out : 'a Run_length_encode.Rle_out.t
    ; bitstream : 'a Bitstream_writer.O.t
    ; done_ : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

val create : Scope.t -> Interface.Create_fn(I)(O).t
val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
