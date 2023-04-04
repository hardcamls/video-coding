open! Base
open Hardcaml

module I : sig
  type 'a t =
    { clocking : 'a Clocking.t
    ; dht_header : 'a Markers.Dht.Header.Fields.t
    ; dht_code : 'a Codeword_decoder.Code.t
    ; dht_code_data : 'a Markers.Dht.Code_data.t
    ; start : 'a
    ; bits : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module O : sig
  type 'a t =
    { done_ : 'a
    ; read_bits : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

val create : Scope.t -> Interface.Create_fn(I)(O).t
