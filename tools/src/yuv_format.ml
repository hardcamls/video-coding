open! Core
open Hardcaml_video_common

module Packed = struct
  type t =
    | YUY2
    | UYVY
    | YVYU
  [@@deriving sexp_of]

  let get_format = function
    | YUY2 -> Packed_422.yuy2
    | UYVY -> Packed_422.uyvy
    | YVYU -> Packed_422.yvyu
  ;;

  let create { Size.width; height } = Plane.create ~width:(width * 2) ~height
  let input file p = Plane.input p file
  let output file p = Plane.output p file
end

module Planar = struct
  type t =
    | C420
    | C422
    | C444
  [@@deriving sexp_of]

  let create { Size.width; height } ~fmt =
    let cwidth =
      match fmt with
      | C420 | C422 -> width / 2
      | C444 -> width
    in
    let cheight =
      match fmt with
      | C420 -> height / 2
      | C422 | C444 -> height
    in
    { Yuv.y = Plane.create ~width ~height
    ; u = Plane.create ~width:cwidth ~height:cheight
    ; v = Plane.create ~width:cwidth ~height:cheight
    }
  ;;

  let input file { Yuv.y; u; v } =
    Plane.input y file;
    Plane.input u file;
    Plane.input v file
  ;;

  let output file { Yuv.y; u; v } =
    Plane.output y file;
    Plane.output u file;
    Plane.output v file
  ;;
end

type ('packed, 'planar) t_poly =
  | Packed of 'packed
  | Planar of 'planar
[@@deriving sexp_of]

type t = (Packed.t, Planar.t) t_poly [@@deriving sexp_of]

let arg_type =
  Command.Arg_type.create (fun s ->
      match String.uppercase s with
      | "420" -> Planar Planar.C420
      | "422" -> Planar Planar.C422
      | "444" -> Planar Planar.C444
      | "YUY2" -> Packed Packed.YUY2
      | "UYVY" -> Packed Packed.UYVY
      | "YVYU" -> Packed Packed.YVYU
      | _ -> raise_s [%message "Invalid YUV format"])
;;
