open Core

type t

val readme : unit -> string
val arg : t Command.Param.t
val main : t -> unit
