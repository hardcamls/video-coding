open Core
open Hardcaml_video_tools

let command_oplay =
  Command.basic
    ~summary:"YUV file player"
    ~readme:Oplay.readme
    [%map_open.Command
      let t = Oplay.arg in
      fun () -> Oplay.main t]
;;

let command_convert =
  Command.basic
    ~summary:"YUV file format/size conversion"
    [%map_open.Command
      let t = Oconv.arg in
      fun () -> Oconv.main t]
;;

let () =
  Command_unix.run
    (Command.group
       ~summary:"YUV file processing tools"
       [ "play", command_oplay; "convert", command_convert ])
;;
