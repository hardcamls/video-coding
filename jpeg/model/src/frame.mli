module Chroma_subsampling : sig
  type t =
    | C420
    | C422
    | C444
end

type t

val create : chroma_subsampling:Chroma_subsampling.t -> width:int -> height:int -> t
val of_planes : y:Plane.t -> u:Plane.t -> v:Plane.t -> t
val width : t -> int
val height : t -> int
val y : t -> Plane.t
val u : t -> Plane.t
val v : t -> Plane.t
val chroma_subsampling : t -> Chroma_subsampling.t
val output : t -> Stdio.Out_channel.t -> unit
val input : t -> Stdio.In_channel.t -> unit
