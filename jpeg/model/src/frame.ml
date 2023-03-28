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

let width t = Plane.width (y t)
let height t = Plane.height (y t)

let output t ~out_channel =
  Plane.output (y t) ~out_channel;
  Plane.output (u t) ~out_channel;
  Plane.output (v t) ~out_channel
;;
