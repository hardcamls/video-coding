open Core

type t

module Arg : sig
  val arg : t Command.Param.t
end

val readme : unit -> string
val main : t -> unit
