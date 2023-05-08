open Core

include struct
  open Hardcaml_jpeg_model
  module Decoder = Decoder
  module Encoder = Encoder
end

include struct
  open Hardcaml_video_common
  module Frame = Frame
  module Plane = Plane
  module Reader = Bitstream_reader
  module Writer = Bitstream_writer
  module Size = Size
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

let input_yuv file ~chroma_subsampling ~width ~height =
  let frame = Frame.create ~chroma_subsampling ~width ~height in
  In_channel.with_file file ~f:(fun file -> Frame.input frame file);
  frame
;;

let chroma_arg =
  Command.Arg_type.create (function
      | "420" -> Frame.Chroma_subsampling.C420
      | "422" -> Frame.Chroma_subsampling.C422
      | "444" -> Frame.Chroma_subsampling.C444
      | _ -> raise_s [%message "Invalid chroma type"])
;;

let command_encode_frame =
  Command.basic
    ~summary:"Encoder a frame"
    [%map_open.Command
      let yuv = anon ("INPUT-FRAME" %: string)
      and { width; height } = anon ("WIDTHxHEIGHT" %: Size.arg_type)
      and bits = anon ("OUTPUT-BITS" %: string)
      and quality = flag "-quality" (optional_with_default 75 int) ~doc:" Image quality"
      and chroma_subsampling =
        flag
          "-chroma"
          (optional_with_default Frame.Chroma_subsampling.C420 chroma_arg)
          ~doc:""
      in
      fun () ->
        let frame = input_yuv yuv ~chroma_subsampling ~width ~height in
        let writer = Writer.create () in
        (match chroma_subsampling with
        | C420 -> Encoder.encode_420 ~frame ~quality ~writer
        | C422 -> Encoder.encode_422 ~frame ~quality ~writer
        | C444 -> Encoder.encode_444 ~frame ~quality ~writer);
        Out_channel.write_all bits ~data:(Writer.get_buffer writer)]
;;

let command_encode_log =
  Command.basic
    ~summary:"Encode a frame and write a log file"
    [%map_open.Command
      let yuv = anon ("INPUT-FRAME" %: string)
      and { width; height } = anon ("WIDTHxHEIGHT" %: Size.arg_type)
      and quality = flag "-quality" (optional_with_default 75 int) ~doc:" Image quality"
      and chroma_subsampling =
        flag
          "-chroma"
          (optional_with_default Frame.Chroma_subsampling.C420 chroma_arg)
          ~doc:""
      and verbose = flag "-verbose" no_arg ~doc:" Reconstruct and compute error" in
      fun () ->
        let frame = Frame.create ~chroma_subsampling ~width ~height in
        In_channel.with_file yuv ~f:(Frame.input frame);
        let writer = Writer.create () in
        let params =
          match chroma_subsampling with
          | C420 -> Encoder.Parameters.c420 ~width ~height ~quality
          | C422 -> Encoder.Parameters.c422 ~width ~height ~quality
          | C444 -> Encoder.Parameters.c444 ~width ~height ~quality
        in
        Encoder.write_headers ~params ~writer;
        let encoder =
          Encoder.create ~compute_reconstruction_error:verbose ~params ~writer ()
        in
        Plane.blit_available ~src:(Frame.y frame) ~dst:(Encoder.get_plane encoder 0);
        Plane.blit_available ~src:(Frame.u frame) ~dst:(Encoder.get_plane encoder 1);
        Plane.blit_available ~src:(Frame.v frame) ~dst:(Encoder.get_plane encoder 2);
        let block_number = ref 0 in
        Sequence.iter (Encoder.encode_seq encoder) ~f:(fun block ->
            print_s [%message (!block_number : int) (block : Encoder.Block.t)]);
        Encoder.complete_and_write_eoi encoder]
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
