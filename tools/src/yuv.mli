open! Core
open Hardcaml_video_common

type t =
  { y : Plane.t
  ; u : Plane.t
  ; v : Plane.t
  }

val create_444 : width:int -> height:int -> t
val create_422 : width:int -> height:int -> t
val create_420 : width:int -> height:int -> t
val crop : x_pos:int -> y_pos:int -> src:t -> dst:t -> unit
val assert_is_444 : t -> unit
val assert_is_422 : t -> unit
val assert_is_420 : t -> unit

module For_testing : sig
  val dump : Plane.t -> unit
  val dump_yuv : t -> unit
end
