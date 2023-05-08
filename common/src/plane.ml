open Base
module Bigstring = Base_bigstring

type t =
  { width : int
  ; height : int
  ; plane : Bigstring.t
  }
[@@deriving fields]

let alloc ~width ~height =
  let bs = Bigstring.create (width * height) in
  for i = 0 to (width * height) - 1 do
    bs.{i} <- '\000'
  done;
  bs
;;

let create ~width ~height = { width; height; plane = alloc ~width ~height }
let blit ~src ~dst = Bigstring.blito ~src:src.plane ~dst:dst.plane ()

let blit_available ~src ~dst =
  let width = min src.width dst.width in
  let height = min src.height dst.height in
  for row = 0 to height - 1 do
    Bigstring.blito
      ~src:src.plane
      ~src_pos:(row * src.width)
      ~src_len:width
      ~dst:dst.plane
      ~dst_pos:(row * dst.width)
      ()
  done
;;

let copy src =
  let dst = create ~width:src.width ~height:src.height in
  blit ~src ~dst;
  dst
;;

let ( .!() ) t index = Bigstring.get t.plane index
let ( .!()<- ) t index value = Bigstring.set t.plane index value

let ( .![] ) t (x, y) =
  if x < 0 || x >= t.width || y < 0 || y >= t.height
  then
    raise_s
      [%message
        "[Plane.get] out of bounds" (x : int) (y : int) (t.width : int) (t.height : int)];
  t.!(x + (y * t.width))
;;

let ( .![]<- ) t (x, y) value =
  if x < 0 || x >= t.width || y < 0 || y >= t.height
  then
    raise_s
      [%message
        "[Plane.set] out of bounds" (x : int) (y : int) (t.width : int) (t.height : int)];
  t.!(x + (y * t.width)) <- value
;;

let output t out_channel =
  for y = 0 to t.height - 1 do
    for x = 0 to t.width - 1 do
      Stdio.Out_channel.output_char out_channel t.![x, y]
    done
  done
;;

exception End_of_image

let input t in_channel =
  for y = 0 to t.height - 1 do
    for x = 0 to t.width - 1 do
      t.![x, y]
        <- (match Stdio.In_channel.input_char in_channel with
           | Some x -> x
           | None -> raise End_of_image)
    done
  done
;;
