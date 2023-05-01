open Base

type t =
  { width : int
  ; height : int
  ; plane : (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
  }
[@@deriving fields]

let create ~width ~height =
  { width
  ; height
  ; plane = Bigarray.Array1.create Bigarray.char Bigarray.c_layout (width * height)
  }
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

let input t in_channel =
  for y = 0 to t.height - 1 do
    for x = 0 to t.width - 1 do
      t.![x, y] <- Stdio.In_channel.input_char in_channel |> Option.value_exn
    done
  done
;;
