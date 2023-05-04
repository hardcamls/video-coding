open Core
open Hardcaml_video_tools

let command_oplay =
  Command.basic
    ~summary:"YUV file player"
    ~readme:Oplay.readme
    [%map_open.Command
      let t = Oplay.Arg.arg in
      fun () -> Oplay.main t]
;;

let () = Command_unix.run command_oplay
