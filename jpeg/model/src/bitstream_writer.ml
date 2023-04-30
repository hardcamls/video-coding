open! Base

type t =
  { mutable word_buffer : int
  ; mutable word_bits : int
  ; buffer : Buffer.t
  ; mutable bytes_written : int
  }
[@@deriving fields]

let create () =
  { word_buffer = 0
  ; word_bits = 0
  ; buffer = Buffer.create (16 * 1024)
  ; bytes_written = 0
  }
;;

let flush t =
  while t.word_bits >= 8 do
    let d = (t.word_buffer lsr (t.word_bits - 8)) land 0xff in
    Buffer.add_char t.buffer (Char.of_int_exn d);
    t.word_bits <- t.word_bits - 8
  done
;;

let put_bits t ~value ~bits =
  assert (bits <= 16);
  t.word_buffer <- (t.word_buffer lsl bits) lor (value land ((1 lsl bits) - 1));
  t.word_bits <- t.word_bits + bits;
  flush t
;;

let get_buffer t = Buffer.contents t.buffer
let bits_written t = (t.bytes_written * 8) + t.word_bits

let flush_with_1s t =
  while bits_written t land 7 <> 0 do
    put_bits t ~value:1 ~bits:1
  done
;;
