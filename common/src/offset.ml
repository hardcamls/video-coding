open Core

type t =
  { x_off : int
  ; y_off : int
  }
[@@deriving sexp_of]

let arg_type =
  Command.Arg_type.create (fun s ->
      try
        match String.split_on_chars ~on:[ 'x'; ','; '-' ] s with
        | [ x_off; y_off ] -> { x_off = Int.of_string x_off; y_off = Int.of_string y_off }
        | _ -> raise Caml.Not_found
      with
      | _ -> raise_s [%message "Invalid frame size specified" (s : string)])
;;
