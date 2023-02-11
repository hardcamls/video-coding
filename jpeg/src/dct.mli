(** IDCT implementation *)

open Base
open Hardcaml

module Single_multiplier : sig
  val input_bits : int
  val transpose_bits : int

  module I : sig
    type 'a t =
      { clocking : 'a Clocking.t
      ; input_coef : 'a
      ; transpose_coef : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { write_enable : 'a
      ; write_address : 'a
      ; read_address : 'a
      ; write_coef : 'a
      ; read_enable : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  val create : Signal.t Interface.Create_fn(I)(O).t
end

module Simple : sig
  module I : sig
    type 'a t = 
      { clocking:'a Clocking.t
      ; start : 'a
      ; coef : 'a
      }
      [@@deriving sexp_of, hardcaml] 
  end

  module O : sig
    type 'a t = 
      { pixel :'a 
      ; pixel_address : 'a
      ; pixel_write : 'a
      ; coef_address : 'a
      ; coef_read : 'a
      }
      [@@deriving sexp_of, hardcaml] 
  end

  val create : Scope.t -> Signal.t Interface.Create_fn(I)(O).t
end