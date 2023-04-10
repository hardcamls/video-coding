(* JPEG core simulations *)
open Core

let command_decode_headers =
  Command.basic
    ~summary:"Header decoding simulation"
    [%map_open.Command
      let jpeg = anon ("JPEG" %: string) in
      fun () ->
        let waves = Hardcaml_jpeg_test.Test_decode_headers.test jpeg in
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

let command_decoder =
  Command.basic
    ~summary:"Decoder simulation"
    [%map_open.Command
      let jpeg = anon ("JPEG" %: string)
      and yuv = flag "-yuv" (optional string) ~doc:""
      and num_blocks_to_decode =
        flag "-blocks" (optional int) ~doc:"Number of blocks to decode in simulation"
      and error_tolerance =
        flag
          "-error-tolerance"
          (optional int)
          ~doc:"Allowable error in reconstructed pixels compare to model reference"
      in
      fun () ->
        let frame, waves =
          Hardcaml_jpeg_test.Test_decoder.test
            ~waves:true
            ?num_blocks_to_decode
            ?error_tolerance
            jpeg
        in
        Option.iter yuv ~f:(fun yuv ->
            Out_channel.with_file yuv ~f:(fun yuv ->
                Hardcaml_jpeg_model.Frame.output ~out_channel:yuv frame));
        Option.iter waves ~f:Hardcaml_waveterm_interactive.run]
;;

let command =
  Command.group
    ~summary:"JPEG core simulations"
    [ "dct", Dct.command
    ; "headers", command_decode_headers
    ; "codeblock", command_codeblock
    ; "decoder", command_decoder
    ]
;;

let () = Command_unix.run command
