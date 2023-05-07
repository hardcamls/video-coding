open Core

type t =
  { start : int
  ; end_ : int
  }
[@@deriving sexp_of]

val arg_type : t Command.Arg_type.t
