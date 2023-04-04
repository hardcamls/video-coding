(* JPEG core simulations *)
open Core

let command_vld =
  Command.basic
    ~summary:"VLD simulation"
    [%map_open.Command
      let jpeg = anon ("JPEG" %: string) in
      fun () ->
        let jpeg = In_channel.with_file jpeg ~f:In_channel.input_all in
        let waves = Hardcaml_jpeg_test.Test_vld.test_vld jpeg in
        Option.iter waves ~f:Hardcaml_waveterm_interactive.run]
;;

let command_codeblock =
  Command.basic
    ~summary:"Codeblock simulation"
    [%map_open.Command
      let jpeg = anon ("JPEG" %: string) in
      fun () ->
        let waves = Hardcaml_jpeg_test.Test_codeblock_decoder.test ~waves:true jpeg in
        Option.iter waves ~f:Hardcaml_waveterm_interactive.run]
;;

let command =
  Command.group
    ~summary:"JPEG core simulations"
    [ "dct", Dct.command; "vld", command_vld; "codeblock", command_codeblock ]
;;

let () = Command_unix.run command
