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
end

(* Up and down convert planar 420/422 to 444 *)
module Planar_444 : sig
  val to_unpacked_420 : Yuv.t -> Yuv.t
  val of_unpacked_420 : Yuv.t -> Yuv.t
  val to_unpacked_422 : Yuv.t -> Yuv.t
  val of_unpacked_422 : Yuv.t -> Yuv.t
end

(* Convert packed to planar 422 *)
module Packed_422 : sig
  type packed_yuv_format

  val yuy2 : packed_yuv_format
  val uyvy : packed_yuv_format
  val yvyu : packed_yuv_format
  val to_planar : Plane.t -> packed_yuv_format -> Yuv.t
  val of_planar : Yuv.t -> packed_yuv_format -> Plane.t
end
