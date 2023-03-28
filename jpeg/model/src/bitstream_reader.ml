open Base

module type Buffer = sig
  type t [@@deriving sexp_of]

  val length : t -> int
  val get : t -> int -> char
end

module Make (Buffer : Buffer) = struct
  type t =
    { buffer : (Buffer.t[@sexp.opaque])
    ; length_in_bits : int
    ; mutable bit_pos : int
    }
  [@@deriving sexp_of]

  let create buffer = { buffer; length_in_bits = Buffer.length buffer * 8; bit_pos = 0 }

  let get_byte t byte_no =
    try Buffer.get t.buffer byte_no with
    | _ -> '\000'
  ;;

  let get_bit t pos =
    let byte_no = pos lsr 3 in
    let bit_no = 7 - (pos land 7) in
    let byte = Char.to_int (get_byte t byte_no) in
    (byte lsr bit_no) land 1
  ;;

  let show t n =
    if n >= t.length_in_bits then raise_s [%message "Bitstream_reader out of bounds"];
    let v = ref 0 in
    for i = 0 to n - 1 do
      v := (!v lsl 1) lor get_bit t (t.bit_pos + i)
    done;
    !v
  ;;

  let advance t n = t.bit_pos <- t.bit_pos + n

  let get t n =
    let v = show t n in
    advance t n;
    v
  ;;

  let bit_pos t = t.bit_pos
  let bits_left t = t.length_in_bits - bit_pos t

  let align_to_byte t =
    let num_bits = bit_pos t land 7 in
    if num_bits <> 0 then advance t (8 - num_bits)
  ;;
end
