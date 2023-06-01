(** Simple Fixde point IDCT implementation.contents
  
    Currently takes 1024 cycles per 8x8 block (it implements 2 8x8 matrix multiplies with a single multiplier.)
*)

open! Base
open Hardcaml

module type Config = sig
  val input_bits : int
  val output_bits : int
  val rom_prec : int
  val transpose_prec : int
  val transform_matrix : float Hardcaml_jpeg_model.Dct.Matrix8x8.t
end

module Dct_config : Config
module Idct_config : Config

module Make (Config : Config) : sig
  val transpose_bits : int

  module I : sig
    type 'a t =
      { clocking : 'a Clocking.t
      ; start : 'a
      ; coef_in : 'a
      ; transpose_coef_in : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { coef_out : 'a
      ; coef_out_write : 'a
      ; transpose_coef_out : 'a
      ; transpose_write : 'a
      ; coef_in_read : 'a
      ; transpose_read : 'a
      ; read_address : 'a
      ; write_address : 'a
      ; done_ : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  val create : Scope.t -> Interface.Create_fn(I)(O).t
  val hierarchical : Scope.t -> Interface.Create_fn(I)(O).t
end
