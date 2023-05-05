(* Convert packed and planar 4:2:0 and 4:2:2 frames to and from 
   4:4:4 sampled YUV frames. 
 
   We use basic averaging assuming co-located luma/chroma pixels which 
   is at best a rough approximation.
*)
open! Core
open Hardcaml_video_common

module Yuv : sig
  type t =
    { y : Plane.t
    ; u : Plane.t
    ; v : Plane.t
    }

  val create_444 : width:int -> height:int -> t
  val create_422 : width:int -> height:int -> t
  val create_420 : width:int -> height:int -> t
end

(* Up and down convert planar 420/422 to 444 *)
module Planar_444 : sig
  val convert_to_420 : src:Yuv.t -> dst:Yuv.t -> unit
  val convert_from_420 : src:Yuv.t -> dst:Yuv.t -> unit
  val convert_to_422 : src:Yuv.t -> dst:Yuv.t -> unit
  val convert_from_422 : src:Yuv.t -> dst:Yuv.t -> unit
  val to_420 : Yuv.t -> Yuv.t
  val of_420 : Yuv.t -> Yuv.t
  val to_422 : Yuv.t -> Yuv.t
  val of_422 : Yuv.t -> Yuv.t
end

(* Convert packed to planar 422 *)
module Packed_422 : sig
  type packed_yuv_format

  val yuy2 : packed_yuv_format
  val uyvy : packed_yuv_format
  val yvyu : packed_yuv_format
  val convert_to_planar : packed_yuv_format -> src:Plane.t -> dst:Yuv.t -> unit
  val convert_from_planar : packed_yuv_format -> src:Yuv.t -> dst:Plane.t -> unit
  val to_planar : packed_yuv_format -> Plane.t -> Yuv.t
  val of_planar : packed_yuv_format -> Yuv.t -> Plane.t
end

type t

val arg : t Command.Param.t
val main : t -> unit
