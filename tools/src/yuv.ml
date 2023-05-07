open! Core
open Hardcaml_video_common

type t =
  { y : Plane.t
  ; u : Plane.t
  ; v : Plane.t
  }

let create_444 ~width ~height =
  { y = Plane.create ~width ~height
  ; u = Plane.create ~width ~height
  ; v = Plane.create ~width ~height
  }
;;

let create_422 ~width ~height =
  { y = Plane.create ~width ~height
  ; u = Plane.create ~width:(width / 2) ~height
  ; v = Plane.create ~width:(width / 2) ~height
  }
;;

let create_420 ~width ~height =
  { y = Plane.create ~width ~height
  ; u = Plane.create ~width:(width / 2) ~height:(height / 2)
  ; v = Plane.create ~width:(width / 2) ~height:(height / 2)
  }
;;

(* let ( .![] ) p (x, y) =
    let x = max 0 (min (Plane.width p - 1) x) in
    let y = max 0 (min (Plane.height p - 1) y) in
    Plane.(p.![x, y])
  ;;

  let ( .![]<- ) p (x, y) value =
    let x = max 0 (min (Plane.width p - 1) x) in
    let y = max 0 (min (Plane.height p - 1) y) in
    Plane.(p.![x, y] <- value)
  ;; *)

let crop ~x_pos ~y_pos ~(src : t) ~(dst : t) =
  let width_dst = Plane.width dst.y in
  let height_dst = Plane.height dst.y in
  let width_src = Plane.width src.y in
  let height_src = Plane.height src.y in
  for row_dst = 0 to height_dst - 1 do
    for col_dst = 0 to width_dst - 1 do
      (* Clip to within src image *)
      let col = col_dst + x_pos in
      let col = if col < 0 then 0 else if col >= width_src then width_src - 1 else col in
      let row = row_dst + y_pos in
      let row =
        if row < 0 then 0 else if row >= height_src then height_src - 1 else row
      in
      Plane.(dst.y.![col_dst, row_dst] <- src.y.![col, row]);
      Plane.(dst.u.![col_dst, row_dst] <- src.u.![col, row]);
      Plane.(dst.v.![col_dst, row_dst] <- src.v.![col, row])
    done
  done
;;

let _pad ~x_pos ~y_pos ~(src : t) ~(dst : t) =
  let width = Plane.width dst.y in
  let height = Plane.height dst.y in
  for row = 0 to height - 1 do
    for col = 0 to width - 1 do
      Plane.(dst.y.![col, row] <- src.y.![col + y_pos, row + x_pos]);
      Plane.(dst.u.![col, row] <- src.u.![col + y_pos, row + x_pos]);
      Plane.(dst.v.![col, row] <- src.v.![col + y_pos, row + x_pos])
    done
  done
;;

let assert_is_444 (yuv : t) =
  let wy, hy = Plane.width yuv.y, Plane.height yuv.y in
  let wu, hu = Plane.width yuv.u, Plane.height yuv.u in
  let wv, hv = Plane.width yuv.v, Plane.height yuv.v in
  if not (wy = wu && wu = wv && hy = hu && hu = hv)
  then
    raise_s
      [%message
        "Expecting a 4:4:4 frame"
          (wy, hy : int * int)
          (wu, hu : int * int)
          (wv, hv : int * int)]
;;

let assert_is_422 (yuv : t) =
  let wy, hy = Plane.width yuv.y, Plane.height yuv.y in
  let wu, hu = Plane.width yuv.u, Plane.height yuv.u in
  let wv, hv = Plane.width yuv.v, Plane.height yuv.v in
  if not (wy = wu * 2 && wu = wv && hy = hu && hu = hv)
  then
    raise_s
      [%message
        "Expecting a 4:2:2 frame"
          (wy, hy : int * int)
          (wu, hu : int * int)
          (wv, hv : int * int)]
;;

let assert_is_420 (yuv : t) =
  let wy, hy = Plane.width yuv.y, Plane.height yuv.y in
  let wu, hu = Plane.width yuv.u, Plane.height yuv.u in
  let wv, hv = Plane.width yuv.v, Plane.height yuv.v in
  if not (wy = wu * 2 && wu = wv && hy = hu * 2 && hu = hv)
  then
    raise_s
      [%message
        "Expecting a 4:2:0 frame"
          (wy, hy : int * int)
          (wu, hu : int * int)
          (wv, hv : int * int)]
;;

module For_testing = struct
  let dump (p : Plane.t) =
    for row = 0 to Plane.height p - 1 do
      for col = 0 to Plane.width p - 1 do
        printf "%3i " (Char.to_int Plane.(p.![col, row]))
      done;
      printf "\n"
    done
  ;;

  let dump_yuv (f : t) =
    dump f.y;
    dump f.u;
    dump f.v
  ;;
end
