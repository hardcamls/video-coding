open! Base
open Hardcaml

module I : sig
  type 'a t =
    { clocking : 'a Clocking.t
    ; dht_header : 'a Markers.Dht.Header.Fields.t
    ; dht_code : 'a Codeword_decoder.Code.t
    ; dht_code_data : 'a Markers.Dht.Code_data.t
    ; start : 'a
    ; table_id : 'a
    ; bits : 'a
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

module O : sig
  type 'a t =
    { done_ : 'a
    ; read_bits : 'a
    ; errors : 'a Errors.t
    }
  [@@deriving sexp_of, hardcaml]
end

val create : Scope.t -> Interface.Create_fn(I)(O).t
val hierarchical : Scope.t -> Interface.Create_fn(I)(O).t