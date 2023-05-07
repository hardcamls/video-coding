open Core

type t =
  { start : int
  ; end_ : int
  }
[@@deriving sexp_of]

let arg_type =
  Command.Arg_type.create (fun s ->
      try
        match String.split_on_chars ~on:[ 'x'; ','; '-' ] s with
        | [ start ] -> { start = Int.of_string start; end_ = Int.of_string start }
        | [ ""; end_ ] -> { start = 0; end_ = Int.of_string end_ }
        | [ start; end_ ] -> { start = Int.of_string start; end_ = Int.of_string end_ }
        | _ -> raise Caml.Not_found
      with
      | _ -> raise_s [%message "Invalid frame size specified" (s : string)])
;;
