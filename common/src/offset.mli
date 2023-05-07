open! Core

type t =
  { x_off : int
  ; y_off : int
  }
[@@deriving sexp_of]

val arg_type : t Command.Arg_type.t
