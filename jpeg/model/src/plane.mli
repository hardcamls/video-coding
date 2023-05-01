type t

val create : width:int -> height:int -> t
val width : t -> int
val height : t -> int
val ( .!() ) : t -> int -> char
val ( .!()<- ) : t -> int -> char -> unit
val ( .![] ) : t -> int * int -> char
val ( .![]<- ) : t -> int * int -> char -> unit
val output : t -> Stdio.Out_channel.t -> unit
val input : t -> Stdio.In_channel.t -> unit
