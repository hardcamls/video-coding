open! Core

type t =
  { width : int
  ; height : int
  }
[@@deriving sexp_of]

let of_string s =
  match List.find Stdsizes.sizes ~f:(fun (s', _, _, _) -> String.equal s s') with
  | Some (_, width, height, _) -> { width; height }
  | None ->
    (match String.split ~on:'x' s with
    | [ width; height ] -> { width = Int.of_string width; height = Int.of_string height }
    | _ -> raise_s [%message "Invalid frame size specified" (s : string)])
;;

let arg_type = Command.Arg_type.create of_string
