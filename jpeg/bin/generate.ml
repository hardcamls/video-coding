open! Core
open Hardcaml

let command_jpeg_decoder_accelerator =
  Command.basic
    ~summary:"Generate RTL for JPEG decoder accelerator"
    [%map_open.Command
      let () = return () in
      fun () ->
        let module Accl = Hardcaml_jpeg.Decoder_accelerator in
        let module Circ = Circuit.With_interface (Accl.I) (Accl.O) in
        let scope = Scope.create () in
        Rtl.print
          Verilog
          ~database:(Scope.circuit_database scope)
          (Circ.create_exn ~name:"decoder_accelerator" (Accl.create scope))]
;;

let command_jpeg_decoder =
  Command.basic
    ~summary:"Generate RTL for JPEG decoder"
    [%map_open.Command
      let () = return () in
      fun () ->
        let module Decoder = Hardcaml_jpeg.Decoder in
        let module Circ = Circuit.With_interface (Decoder.I) (Decoder.O) in
        let scope = Scope.create () in
        Rtl.print
          Verilog
          ~database:(Scope.circuit_database scope)
          (Circ.create_exn ~name:"decoder" (Decoder.create scope))]
;;

let command_bytestream_reader =
  Command.basic
    ~summary:"Generate RTL for byte stream decoder"
    [%map_open.Command
      let () = return () in
      fun () ->
        let module Reader = Hardcaml_jpeg.Bytestream_decoder in
        let module Circ = Circuit.With_interface (Reader.I) (Reader.O) in
        let scope = Scope.create () in
        Rtl.print
          Verilog
          ~database:(Scope.circuit_database scope)
          (Circ.create_exn ~name:"bytestream_decoder" (Reader.create scope))]
;;

let () =
  Command_unix.run
    (Command.group
       ~summary:""
       [ "accelerator", command_jpeg_decoder_accelerator
       ; "decoder", command_jpeg_decoder
       ; "bytestream-reader", command_bytestream_reader
       ])
;;
