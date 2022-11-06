open Core
open Hardcaml_jpeg_model

let command_decode_header =
  Command.basic
    ~summary:"Decode a frame header"
    [%map_open.Command
      let bits = anon ("INPUT" %: string) in
      fun () ->
        let bits = Model.Bits.create (In_channel.read_all bits) in
        let header = Model.Header.decode bits Model.Header.empty in
        print_s [%message (header : Model.Header.t)]]
;;

let command_decode_frame =
  Command.basic
    ~summary:"Decode a frame to YUV"
    [%map_open.Command
      let bits = anon ("INPUT" %: string)
      and yuv = anon (maybe ("OUTPUT" %: string)) in
      fun () ->
        let bits = Model.Bits.create (In_channel.read_all bits) in
        let planes = Model.decode_frame bits in
        let yuv =
          match yuv with
          | None -> Out_channel.stdout
          | Some yuv -> Out_channel.create yuv
        in
        Array.iter planes ~f:(fun plane -> Plane.output plane ~out_channel:yuv)]
;;

let () =
  Command_unix.run
    (Command.group
       ~summary:"JPEG decoder model"
       [ "frame", command_decode_frame; "header", command_decode_header ])
;;
