open Core
open Hardcaml_jpeg_model

let command_decode_header =
  Command.basic
    ~summary:"Decode a frame header"
    [%map_open.Command
      let bits = anon ("INPUT" %: string) in
      fun () ->
        let bits = Decoder.Bits.create (In_channel.read_all bits) in
        let header = Decoder.Header.decode bits in
        print_s [%message (header : Decoder.Header.t)]]
;;

let command_decode_frame =
  Command.basic
    ~summary:"Decode a frame to YUV"
    [%map_open.Command
      let bits = anon ("INPUT" %: string)
      and yuv = anon (maybe ("OUTPUT" %: string)) in
      fun () ->
        let bits = Decoder.Bits.create (In_channel.read_all bits) in
        let frame = Decoder.decode_a_frame bits in
        let yuv =
          match yuv with
          | None -> Out_channel.stdout
          | Some yuv -> Out_channel.create yuv
        in
        Frame.output frame ~out_channel:yuv]
;;

let command_decode_log =
  Command.basic
    ~summary:"Decode a frame and write a log file"
    [%map_open.Command
      let bits = anon ("INPUT" %: string) in
      fun () ->
        let bits = Decoder.Bits.create (In_channel.read_all bits) in
        let header = Decoder.Header.decode bits in
        print_s [%message (header : Decoder.Header.t)];
        let decoder = Decoder.init header bits in
        let decoded = Decoder.For_testing.Sequenced.decode decoder in
        let block_number = ref 0 in
        let rec decode decoded =
          match Sequence.hd decoded with
          | None -> (* done *) ()
          | Some component ->
            print_s
              [%message (!block_number : int) (component : Decoder.Component.Summary.t)];
            Int.incr block_number;
            decode (Sequence.tl_eagerly_exn decoded)
        in
        decode decoded]
;;

let () =
  Command_unix.run
    (Command.group
       ~summary:"JPEG decoder model"
       [ "header", command_decode_header
       ; "frame", command_decode_frame
       ; "log", command_decode_log
       ])
;;
