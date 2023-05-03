open Core

include struct
  open Hardcaml_jpeg_model
  module Decoder = Decoder
  module Encoder = Encoder
end

include struct
  open Hardcaml_video_common
  module Frame = Frame
  module Reader = Bitstream_reader
  module Writer = Bitstream_writer
end

let command_decode_header =
  Command.basic
    ~summary:"Decode a frame header"
    [%map_open.Command
      let bits = anon ("INPUT-BITS" %: string) in
      fun () ->
        let bits = Decoder.Bits.create (In_channel.read_all bits) in
        let header = Decoder.Header.decode bits in
        print_s [%message (header : Decoder.Header.t)]]
;;

let command_decode_frame =
  Command.basic
    ~summary:"Decode a frame to YUV"
    [%map_open.Command
      let bits = anon ("INPUT-BITS" %: string)
      and yuv = anon (maybe ("OUTPUT-FRAME" %: string)) in
      fun () ->
        let bits = Decoder.Bits.create (In_channel.read_all bits) in
        let frame = Decoder.decode_a_frame bits in
        let yuv =
          match yuv with
          | None -> Out_channel.stdout
          | Some yuv -> Out_channel.create yuv
        in
        Frame.output frame yuv]
;;

let command_decode_log =
  Command.basic
    ~summary:"Decode a frame and write a log file"
    [%map_open.Command
      let bits = anon ("INPUT-BITS" %: string) in
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

module Size = struct
  type t =
    { width : int
    ; height : int
    }

  let arg =
    Command.Arg_type.create (fun s ->
        match String.split ~on:'x' s with
        | [ width; height ] ->
          { width = Int.of_string width; height = Int.of_string height }
        | _ -> failwith "Invalid size specification")
  ;;
end

let command_encode_frame =
  Command.basic
    ~summary:"Encoder a frame"
    [%map_open.Command
      let yuv = anon ("INPUT-FRAME" %: string)
      and { width; height } = anon ("WIDTHxHEIGHT" %: Size.arg)
      and bits = anon ("OUTPUT-BITS" %: string)
      and quality =
        flag "-quality" (optional_with_default 75 int) ~doc:" Image quality"
      in
      fun () ->
        let frame = Frame.create ~chroma_subsampling:C420 ~width ~height in
        In_channel.with_file yuv ~f:(Frame.input frame);
        let writer = Writer.create () in
        Encoder.encode_420 ~frame ~quality ~writer;
        Out_channel.write_all bits ~data:(Writer.get_buffer writer)]
;;

let command_encode_log =
  Command.basic
    ~summary:"Encode a frame and write a log file"
    [%map_open.Command
      let yuv = anon ("INPUT-FRAME" %: string)
      and { width; height } = anon ("WIDTHxHEIGHT" %: Size.arg)
      and quality = flag "-quality" (optional_with_default 75 int) ~doc:" Image quality"
      and verbose = flag "-verbose" no_arg ~doc:" Reconstruct and compute error" in
      fun () ->
        let frame = Frame.create ~chroma_subsampling:C420 ~width ~height in
        In_channel.with_file yuv ~f:(Frame.input frame);
        let writer = Writer.create () in
        let encoder =
          Encoder.For_testing.Sequenced.create_and_write_header
            ~compute_reconstruction_error:verbose
            ~frame
            ~quality
            ~writer
            ()
        in
        let block_number = ref 0 in
        Sequence.iter
          (Encoder.For_testing.Sequenced.encode_420_seq encoder)
          ~f:(fun block ->
            print_s [%message (!block_number : int) (block : Encoder.Block.t)]);
        Encoder.For_testing.Sequenced.complete_and_write_eoi encoder]
;;

let () =
  Command_unix.run
    (Command.group
       ~summary:"JPEG encoder and decoder models"
       [ ( "decode"
         , Command.group
             ~summary:"Decoder model"
             [ "frame", command_decode_frame
             ; "header", command_decode_header
             ; "log", command_decode_log
             ] )
       ; ( "encode"
         , Command.group
             ~summary:"Encoder model"
             [ "frame", command_encode_frame; "log", command_encode_log ] )
       ])
;;
