(** Huffman decodes the dc and ac coefficients of one 8x8 block.  
    Worst case it should take ~128 cycles, though typically it will be much less. 
*)

open! Base
open Hardcaml

module I : sig
  type 'a t =
    { clocking : 'a Clocking.t
    ; dht : 'a Markers.Dht.Fields.t
    ; start : 'a
    ; table_id : 'a
    ; bits : 'a
    ; bits_valid : 'a
    ; dc_pred : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module Errors : sig
  type 'a t =
    { dc_coef_decode : 'a
    ; ac_coef_decode : 'a
    ; too_many_ac_coefs : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module Idct_coefs : sig
  type 'a t =
    { coef : 'a
    ; address : 'a
    ; write : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module O : sig
  type 'a t =
    { done_ : 'a
    ; read_bits : 'a
    ; idct_coefs : 'a Idct_coefs.t
    ; errors : 'a Errors.t
    ; write_dc_pred : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

val create : Scope.t -> Interface.Create_fn(I)(O).t
val hierarchical : Scope.t -> Interface.Create_fn(I)(O).t

module For_testing : sig
  val decode_magnitude : Bits.t -> Bits.t -> Bits.t
end
