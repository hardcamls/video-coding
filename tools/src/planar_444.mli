(* Up and down convert planar 420/422 to 444 *)

open! Core

val convert_to_422 : src:Yuv.t -> dst:Yuv.t -> unit
val convert_from_422 : src:Yuv.t -> dst:Yuv.t -> unit
val convert_to_420 : src:Yuv.t -> dst:Yuv.t -> unit
val convert_from_420 : src:Yuv.t -> dst:Yuv.t -> unit
val to_422 : Yuv.t -> Yuv.t
val of_422 : Yuv.t -> Yuv.t
val to_420 : Yuv.t -> Yuv.t
val of_420 : Yuv.t -> Yuv.t
