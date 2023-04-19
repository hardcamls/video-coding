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

let () = Command_unix.run command_jpeg_decoder_accelerator
