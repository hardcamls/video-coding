(** Bitstream writer.  Allows writing upto 16 bits into an encoded bitstream. *)

type t

val create : unit -> t
val put_bits : t -> stuffing:bool -> value:int -> bits:int -> unit
val get_buffer : t -> string
val bits_written : t -> int
val bytes_written : t -> int
val flush_with_1s : t -> stuffing:bool -> unit
