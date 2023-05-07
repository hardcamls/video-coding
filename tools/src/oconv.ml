open! Core
open Hardcaml_video_common
open Yuv_format

let create_pipe size fmt =
  match fmt with
  | Packed fmt -> Packed (fmt, size, Packed.create size, Planar.create size ~fmt:C422)
  | Planar fmt -> Planar (fmt, size, Planar.create size ~fmt)
;;

(* Read frame and convert to y444 format. *)
let input file y444 = function
  | Packed (fmt, _, src, y422) ->
    Packed.input file src;
    Packed_422.convert_to_planar (Packed.get_format fmt) ~src ~dst:y422;
    Planar_444.convert_from_422 ~src:y422 ~dst:y444
  | Planar (Planar.C420, _, src) ->
    Yuv.assert_is_420 src;
    Planar.input file src;
    Planar_444.convert_from_420 ~src ~dst:y444
  | Planar (Planar.C422, _, src) ->
    Planar.input file src;
    Planar_444.convert_from_422 ~src ~dst:y444
  | Planar (Planar.C444, _, _) -> Planar.input file y444
;;

let input file y444 i =
  try
    input file y444 i;
    true
  with
  | Plane.End_of_image -> false
;;

let output file y444 = function
  | Packed (fmt, _, dst, y422) ->
    Planar_444.convert_to_422 ~src:y444 ~dst:y422;
    Packed_422.convert_from_planar (Packed.get_format fmt) ~src:y422 ~dst;
    Packed.output file dst
  | Planar (Planar.C420, _, dst) ->
    Planar_444.convert_to_420 ~src:y444 ~dst;
    Planar.output file dst
  | Planar (Planar.C422, _, dst) ->
    Planar_444.convert_to_422 ~src:y444 ~dst;
    Planar.output file dst
  | Planar (Planar.C444, _, _) -> Planar.output file y444
;;

type frame =
  { format : Yuv_format.t
  ; size : Size.t
  }
[@@deriving sexp_of]

type t =
  { in_file : string
  ; out_file : string
  ; src : frame
  ; src_offset : Offset.t
  ; dst : frame
  ; frames : Range.t
  }
[@@deriving sexp_of]

let default_format = Yuv_format.Planar Yuv_format.Planar.C420

let arg =
  [%map_open.Command
    let in_file = anon ("IN-FILE" %: string)
    and size_in = anon ("IN-SIZE" %: Size.arg_type)
    and out_file = anon ("OUT-FILE" %: string)
    and size_out = anon (maybe ("OUT-SIZE" %: Size.arg_type))
    and frames =
      flag
        "-frames"
        (optional_with_default { Range.start = 0; end_ = 0 } Range.arg_type)
        ~doc:""
    and format_in =
      flag "-format" (optional_with_default default_format Yuv_format.arg_type) ~doc:""
    and format_out = flag "-out-format" (optional Yuv_format.arg_type) ~doc:""
    and src_offset =
      flag
        "-src-offset"
        (optional_with_default { Offset.x_off = 0; y_off = 0 } Offset.arg_type)
        ~doc:""
    in
    { in_file
    ; out_file
    ; src = { format = format_in; size = size_in }
    ; src_offset
    ; dst =
        { format = Option.value ~default:format_in format_out
        ; size = Option.value ~default:size_in size_out
        }
    ; frames
    }]
;;

let with_in_file file ~f =
  match file with
  | "-" -> f In_channel.stdin
  | _ -> In_channel.with_file file ~f
;;

let with_out_file file ~f =
  match file with
  | "-" -> f Out_channel.stdout
  | _ -> Out_channel.with_file file ~f
;;

let main t =
  let input_pipe = create_pipe t.src.size t.src.format in
  let input_frame = Yuv.create_444 ~width:t.src.size.width ~height:t.src.size.height in
  let output_pipe = create_pipe t.dst.size t.dst.format in
  let output_frame = Yuv.create_444 ~width:t.dst.size.width ~height:t.dst.size.height in
  with_in_file t.in_file ~f:(fun in_file ->
      with_out_file t.out_file ~f:(fun out_file ->
          With_return.with_return (fun { return } ->
              for _ = 0 to t.frames.start - 1 do
                if not (input in_file input_frame input_pipe) then return ()
              done;
              for _ = 0 to t.frames.end_ - t.frames.start do
                if input in_file input_frame input_pipe
                then (
                  Yuv.crop
                    ~x_pos:t.src_offset.x_off
                    ~y_pos:t.src_offset.y_off
                    ~src:input_frame
                    ~dst:output_frame;
                  output out_file output_frame output_pipe)
                else return ()
              done)))
;;
