(** A single plane within a YUV frame. 
    
    Represented as a Bigstring so limited to 8 bit pixels.
*)

type t

(** Create a [width*height] block of memory to hold the pixel data. *)
val create : width:int -> height:int -> t

(** Create a copy *)
val copy : t -> t

val blit : src:t -> dst:t -> unit

(** Plane width *)
val width : t -> int

(** Plane height *)
val height : t -> int

(** {2 1d accessors} *)

val ( .!() ) : t -> int -> char
val ( .!()<- ) : t -> int -> char -> unit

(** {2 1d accessors} *)

val ( .![] ) : t -> int * int -> char
val ( .![]<- ) : t -> int * int -> char -> unit

(** {2 File IO} *)

(** Write plane to a file *)
val output : t -> Stdio.Out_channel.t -> unit

(** Read plane from a file. *)
val input : t -> Stdio.In_channel.t -> unit
