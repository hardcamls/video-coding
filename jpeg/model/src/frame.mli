(** YUV frames with different chroma sub-sampling factors. *)

module Chroma_subsampling : sig
  type t =
    | C420
    | C422
    | C444
end

type t

(** Create a frame with the given size and chroma subsampling, made up of 3 [Plane.t]s. *)
val create : chroma_subsampling:Chroma_subsampling.t -> width:int -> height:int -> t

(** Create from from consituent [Plane]s.  Chroma mode is inferred. *)
val of_planes : y:Plane.t -> u:Plane.t -> v:Plane.t -> t

(** Width of luma plane *)
val width : t -> int

(** Width of luma plane *)
val height : t -> int

(** Chroma subsampling format. *)
val chroma_subsampling : t -> Chroma_subsampling.t

(** {2 Plane accessors}. *)

val y : t -> Plane.t
val u : t -> Plane.t
val v : t -> Plane.t

(** {2 File IO} *)

val output : t -> Stdio.Out_channel.t -> unit
val input : t -> Stdio.In_channel.t -> unit
