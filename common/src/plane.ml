open Base

type t =
  { width : int
  ; height : int
  ; plane : (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
  }
[@@deriving fields]

let alloc ~width ~height =
  Bigarray.Array1.create Bigarray.char Bigarray.c_layout (width * height)
;;

let create ~width ~height = { width; height; plane = alloc ~width ~height }
let blit ~src ~dst = Bigarray.Array1.blit src.plane dst.plane

let copy src =
  let dst = create ~width:src.width ~height:src.height in
  blit ~src ~dst;
  dst
;;

let ( .!() ) t index = Bigarray.Array1.get t.plane index
let ( .!()<- ) t index value = Bigarray.Array1.set t.plane index value
let ( .![] ) t (x, y) = t.!(x + (y * t.width))
let ( .![]<- ) t (x, y) value = t.!(x + (y * t.width)) <- value

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
