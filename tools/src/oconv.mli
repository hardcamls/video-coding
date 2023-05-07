(* Convert packed and planar 4:2:0 and 4:2:2 frames to and from 
   4:4:4 sampled YUV frames. 
 
   We use basic averaging assuming co-located luma/chroma pixels which 
   is at best a rough approximation.
*)
open! Core

type t

val arg : t Command.Param.t
val main : t -> unit
