open! Core
open Hardcaml_video_common

val max_difference : Plane.t -> Plane.t -> int
val total_difference : Plane.t -> Plane.t -> int
val mean_difference : Plane.t -> Plane.t -> float
val square_error : Plane.t -> Plane.t -> int
val mean_square_error : Plane.t -> Plane.t -> float
val psnr : ?r:float -> Plane.t -> Plane.t -> float
val commands : Command.t
