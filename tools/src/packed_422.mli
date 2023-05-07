(* Convert packed to planar 422 *)

open! Core
open Hardcaml_video_common

type packed_yuv_format = int array

val yuy2 : int array
val uyvy : int array
val yvyu : int array
val convert_to_planar : int array -> src:Plane.t -> dst:Yuv.t -> unit
val to_planar : int array -> Plane.t -> Yuv.t
val convert_from_planar : int array -> src:Yuv.t -> dst:Plane.t -> unit
val of_planar : int array -> Yuv.t -> Plane.t
