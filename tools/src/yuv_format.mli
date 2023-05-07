open Core
open Hardcaml_video_common

module Packed : sig
  type t =
    | YUY2
    | UYVY
    | YVYU
  [@@deriving sexp_of]

  val get_format : t -> int array
  val create : Size.t -> Plane.t
  val input : In_channel.t -> Plane.t -> unit
  val output : Out_channel.t -> Plane.t -> unit
end

module Planar : sig
  type t =
    | C420
    | C422
    | C444
  [@@deriving sexp_of]

  val create : Size.t -> fmt:t -> Yuv.t
  val input : In_channel.t -> Yuv.t -> unit
  val output : Out_channel.t -> Yuv.t -> unit
end

type ('packed, 'planar) t_poly =
  | Packed of 'packed
  | Planar of 'planar
[@@deriving sexp_of]

type t = (Packed.t, Planar.t) t_poly [@@deriving sexp_of]

val arg_type : t Core.Command.Arg_type.t
