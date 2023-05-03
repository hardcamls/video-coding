open Base

module Chroma_subsampling = struct
  type t =
    | C420
    | C422
    | C444

  let width c w =
    match c with
    | C420 -> w / 2
    | C422 -> w / 2
    | C444 -> w
  ;;

  let height c h =
    match c with
    | C420 -> h / 2
    | C422 -> h
    | C444 -> h
  ;;
end

type t =
  { y : Plane.t
  ; u : Plane.t
  ; v : Plane.t
  ; chroma_subsampling : Chroma_subsampling.t
  }
[@@deriving fields]

let create ~chroma_subsampling ~width ~height =
  let cwidth = Chroma_subsampling.width chroma_subsampling width in
  let cheight = Chroma_subsampling.height chroma_subsampling height in
  { y = Plane.create ~width ~height
  ; u = Plane.create ~width:cwidth ~height:cheight
  ; v = Plane.create ~width:cwidth ~height:cheight
  ; chroma_subsampling
  }
;;

let infer_chroma_subsampling y u v : Chroma_subsampling.t =
  if Plane.width u <> Plane.width v || Plane.height u <> Plane.height v
  then raise_s [%message "Chroma planes must be same width and height"];
  let check subsampling_factor =
    Chroma_subsampling.width subsampling_factor (Plane.width y) = Plane.width u
    && Chroma_subsampling.height subsampling_factor (Plane.height y) = Plane.height u
  in
  if check C420
  then C420
  else if check C422
  then C422
  else if check C444
  then C444
  else raise_s [%message "Could not infer chroma subsampling"]
;;

let of_planes ~y ~u ~v =
  let chroma_subsampling = infer_chroma_subsampling y u v in
  { y; u; v; chroma_subsampling }
;;

let width t = Plane.width (y t)
let height t = Plane.height (y t)

let output t out_channel =
  Plane.output (y t) out_channel;
  Plane.output (u t) out_channel;
  Plane.output (v t) out_channel
;;

let input t in_channel =
  Plane.input (y t) in_channel;
  Plane.input (u t) in_channel;
  Plane.input (v t) in_channel
;;
