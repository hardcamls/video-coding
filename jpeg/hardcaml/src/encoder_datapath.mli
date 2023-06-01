open! Base
open! Hardcaml

module I : sig
  type 'a t =
    { clocking : 'a Clocking.t
    ; start_dct : 'a
    ; start_vlc : 'a
    ; pixel : 'a
    ; pixel_write_address : 'a
    ; pixel_write_enable : 'a
    ; quant_write : 'a Quant.Quant_write.t
    }
  [@@deriving sexp_of, hardcaml]
end

module O : sig
  type 'a t =
    { q : 'a
    ; done_ : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

val create : Scope.t -> Interface.Create_fn(I)(O).t
