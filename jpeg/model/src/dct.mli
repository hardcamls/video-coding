(** Forward (type II) and inverse (type III) 8x8 DCTs. *)

(** Chen DCT using integer arithmentic. 

    The forward transform output is scaled by a factor of 4 from the reference 
    dct result.
*)
module Chen : sig
  val forward_8x8 : int array -> unit
  val inverse_8x8 : int array -> unit
end

(** Utilities for dealing witg 8x8 matrices. *)
module Matrix8x8 : sig
  type 'a t = 'a array array [@@deriving sexp_of]

  val init : (row:int -> col:int -> 'a) -> 'a t
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  val mapi : 'a t -> f:(row:int -> col:int -> 'a -> 'b) -> 'b t
  val iter : 'a t -> f:('a -> unit) -> unit
  val iteri : 'a t -> f:(row:int -> col:int -> 'a -> unit) -> unit
  val transpose : 'a t -> 'a t
  val fmul : float t -> float t -> float t
  val imul : int t -> int t -> int t
  val iclip : min:int -> max:int -> int t -> int t
end

module Matrix4x4 : sig
  type 'a t = 'a array array [@@deriving sexp_of]
end

(** Floating point reference DCTs. *)
module Floating_point : sig
  (** Reference 8x8 transforms. *)
  module Eight_point : sig
    val forward_transform_matrix : float Matrix8x8.t
    val inverse_transform_matrix : float Matrix8x8.t
    val forward_transform : float Matrix8x8.t -> float Matrix8x8.t
    val inverse_transform : float Matrix8x8.t -> float Matrix8x8.t
  end

  (** Building 8x8 transforms from 4x4 transforms. *)
  module Using_four_point : sig
    val even_fdct_4pt_coefs : float Matrix4x4.t
    val odd_fdct_4pt_coefs : float Matrix4x4.t
    val even_idct_4pt_coefs : float Matrix4x4.t
    val odd_idct_4pt_coefs : float Matrix4x4.t
    val forward_transform : float Matrix8x8.t -> float Matrix8x8.t
    val inverse_transform : float Matrix8x8.t -> float Matrix8x8.t
  end
end

(** Fixed point approximations of the floating point DCTs. *)
module Fixed_point : sig
  val fixed_coefs : fixed_prec:int -> float Matrix8x8.t -> int Matrix8x8.t
  val round : int -> fixed_prec:int -> int
  val round_matrix : int Matrix8x8.t -> prec:int -> int Matrix8x8.t

  val forward_transform
    :  rom_prec:int
    -> transpose_prec:int
    -> int array array
    -> int array array

  val inverse_transform
    :  rom_prec:int
    -> transpose_prec:int
    -> int array array
    -> int array array
end
