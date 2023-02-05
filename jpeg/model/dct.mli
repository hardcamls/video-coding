(** Forward (type II) and inverse (type III) 8x8 DCTs. *)

module Chen : sig
  val forward_8x8 : int array -> unit
  val inverse_8x8 : int array -> unit
end

module Reference : sig
  (** 8x8 matrix *)
  type 'a matrix8x8 = 'a array array

  (** 4x4 matrix*)
  type 'a matrix4x4 = 'a array array

  module Util : sig
    val init : (row:int -> col:int -> 'a) -> 'a matrix8x8
    val map : 'a matrix8x8 -> f:('a -> 'b) -> 'b matrix8x8
    val map2 : 'a matrix8x8 -> 'b matrix8x8 -> f:('a -> 'b -> 'c) -> 'c matrix8x8
    val mapi : 'a matrix8x8 -> f:(row:int -> col:int -> 'a -> 'b) -> 'b matrix8x8
    val iter : 'a matrix8x8 -> f:('a -> unit) -> unit
    val iteri : 'a matrix8x8 -> f:(row:int -> col:int -> 'a -> unit) -> unit
    val transpose : float matrix8x8 -> float matrix8x8
    val mul : float matrix8x8 -> float matrix8x8 -> float matrix8x8
  end

  (** Reference 8x8 transforms. *)
  module Eight_point : sig
    val forward_transform_matrix : float matrix8x8
    val inverse_transform_matrix : float matrix8x8
    val forward_transform : float matrix8x8 -> float matrix8x8
    val inverse_transform : float matrix8x8 -> float matrix8x8
  end

  (** Building 8x8 transforms from 4x4 transforms. *)
  module Using_four_point : sig
    val even_fdct_4pt_coefs : float matrix4x4
    val odd_fdct_4pt_coefs : float matrix4x4
    val even_idct_4pt_coefs : float matrix4x4
    val odd_idct_4pt_coefs : float matrix4x4
    val forward_transform : float matrix8x8 -> float matrix8x8
    val inverse_transform : float matrix8x8 -> float matrix8x8
  end
end
