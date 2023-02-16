open! Core

let command_dct =
  Command.basic
    ~summary:""
    [%map_open.Command
      let () = return () in
      fun () -> ()]
;;
