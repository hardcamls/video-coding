open! Base
open Hardcaml

module Core : sig
  module I : sig
    type 'a t =
      { clocking : 'a Clocking.t
      ; start : 'a
      ; markers : 'a Decode_headers.All_markers.t
      ; bits : 'a
      ; bits_valid : 'a
      ; pixel_read_address : 'a
      ; pixel_read_enable : 'a
      ; output_done : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { pixel : 'a
      ; read_bits : 'a
      ; start_output : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  val create : Scope.t -> Interface.Create_fn(I)(O).t
  val hierarchical : Scope.t -> Interface.Create_fn(I)(O).t
end
