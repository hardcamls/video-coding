(* JPEG core simulations *)
open Core

let command_codeblock =
  Command.basic
    ~summary:"Codeblock simulation"
    [%map_open.Command
      let jpeg = anon ("JPEG" %: string) in
      fun () ->
        let waves = Hardcaml_jpeg_test.Test_codeblock_decoder.test ~waves:true jpeg in
        Option.iter waves ~f:Hardcaml_waveterm_interactive.run]
;;

let command_bytestream_decoder =
  Command.basic
    ~summary:"Bytestream decoder simulation"
    [%map_open.Command
      let jpeg = anon ("JPEG" %: string) in
      fun () ->
        let waves =
          Hardcaml_jpeg_test.Test_bytestream_decoder.test
            ~waves:true
            ~random_ready:true
            jpeg
        in
        Option.iter waves ~f:Hardcaml_waveterm_interactive.run]
;;

let command_filter_stuffed_bytes =
  Command.basic
    ~summary:"stuffed bytes simulation"
    [%map_open.Command
      let size = anon ("NUM-BYTES" %: int)
      and verbose = flag "-verbose" no_arg ~doc:""
      and waves = flag "-waves" no_arg ~doc:""
      and seed = flag "-seed" (optional int) ~doc:""
      and min = flag "-min" (optional int) ~doc:"" in
      fun () ->
        Option.iter seed ~f:Random.init;
        let waves =
          Hardcaml_jpeg_test.Test_filter_stuffed_bytes.regression
            ~waves
            ~verbose
            ?min
            size
        in
        Option.iter
          waves
          ~f:
            (Hardcaml_waveterm_interactive.run
               ~display_rules:Hardcaml_jpeg_test.Test_filter_stuffed_bytes.display_rules)]
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
      and waves = flag "-waves" no_arg ~doc:"Show waveform" in
      fun () ->
        let frame, waves =
          Hardcaml_jpeg_test.Test_decoder.test
            ~waves
            ?num_blocks_to_decode
            ?error_tolerance
            jpeg
        in
        Option.iter yuv ~f:(fun yuv ->
            Out_channel.with_file yuv ~f:(fun yuv ->
                Hardcaml_jpeg_model.Frame.output ~out_channel:yuv frame));
        Option.iter waves ~f:Hardcaml_waveterm_interactive.run]
;;

let command_decoder_accelerator =
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
      and waves = flag "-waves" no_arg ~doc:"Show waveform" in
      fun () ->
        let frame, waves =
          Hardcaml_jpeg_test.Test_decoder_accelerator.test
            ~waves
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
    ; "codeblock", command_codeblock
    ; "bytestream-decoder", command_bytestream_decoder
    ; "decoder", command_decoder
    ; "accelerator", command_decoder_accelerator
    ; "filter-stuffed-bytes", command_filter_stuffed_bytes
    ]
;;

let () = Command_unix.run command
