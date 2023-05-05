open! Core

type t =
  { width : int
  ; height : int
  }
[@@deriving sexp_of]

val of_string : string -> t
val arg_type : t Command.Arg_type.t
