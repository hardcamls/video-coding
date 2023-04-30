type t

val create : unit -> t
val put_bits : t -> value:int -> bits:int -> unit
val get_buffer : t -> string
val bits_written : t -> int
val bytes_written : t -> int
val flush_with_1s : t -> unit
