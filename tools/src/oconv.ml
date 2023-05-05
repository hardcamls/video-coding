open! Core
open Hardcaml_video_common

module Yuv = struct
  type t =
    { y : Plane.t
    ; u : Plane.t
    ; v : Plane.t
    }

  let create_444 ~width ~height =
    { y = Plane.create ~width ~height
    ; u = Plane.create ~width ~height
    ; v = Plane.create ~width ~height
    }
  ;;

  let create_422 ~width ~height =
    { y = Plane.create ~width ~height
    ; u = Plane.create ~width:(width / 2) ~height
    ; v = Plane.create ~width:(width / 2) ~height
    }
  ;;

  let create_420 ~width ~height =
    { y = Plane.create ~width ~height
    ; u = Plane.create ~width:(width / 2) ~height:(height / 2)
    ; v = Plane.create ~width:(width / 2) ~height:(height / 2)
    }
  ;;

  let dump (p : Plane.t) =
    for row = 0 to Plane.height p - 1 do
      for col = 0 to Plane.width p - 1 do
        printf "%3i " (Char.to_int Plane.(p.![col, row]))
      done;
      printf "\n"
    done
  ;;

  let dump_yuv (f : t) =
    dump f.y;
    dump f.u;
    dump f.v
  ;;

  (* let ( .![] ) p (x, y) =
    let x = max 0 (min (Plane.width p - 1) x) in
    let y = max 0 (min (Plane.height p - 1) y) in
    Plane.(p.![x, y])
  ;;

  let ( .![]<- ) p (x, y) value =
    let x = max 0 (min (Plane.width p - 1) x) in
    let y = max 0 (min (Plane.height p - 1) y) in
    Plane.(p.![x, y] <- value)
  ;; *)

  let crop ~x_pos ~y_pos ~(src : t) ~(dst : t) =
    let width_dst = Plane.width dst.y in
    let height_dst = Plane.height dst.y in
    let width_src = Plane.width src.y in
    let height_src = Plane.height src.y in
    for row_dst = 0 to height_dst - 1 do
      for col_dst = 0 to width_dst - 1 do
        (* Clip to within src image *)
        let col = col_dst + x_pos in
        let col =
          if col < 0 then 0 else if col >= width_src then width_src - 1 else col
        in
        let row = row_dst + y_pos in
        let row =
          if row < 0 then 0 else if row >= height_src then height_src - 1 else row
        in
        Plane.(dst.y.![col_dst, row_dst] <- src.y.![col, row]);
        Plane.(dst.u.![col_dst, row_dst] <- src.u.![col, row]);
        Plane.(dst.v.![col_dst, row_dst] <- src.v.![col, row])
      done
    done
  ;;

  let _pad ~x_pos ~y_pos ~(src : t) ~(dst : t) =
    let width = Plane.width dst.y in
    let height = Plane.height dst.y in
    for row = 0 to height - 1 do
      for col = 0 to width - 1 do
        Plane.(dst.y.![col, row] <- src.y.![col + y_pos, row + x_pos]);
        Plane.(dst.u.![col, row] <- src.u.![col + y_pos, row + x_pos]);
        Plane.(dst.v.![col, row] <- src.v.![col + y_pos, row + x_pos])
      done
    done
  ;;
end

module Planar_444 = struct
  let avg2 a b =
    let a = Char.to_int a in
    let b = Char.to_int b in
    Char.of_int_exn ((a + b + 1) lsr 1)
  ;;

  let avg4 a b c d =
    let a = Char.to_int a in
    let b = Char.to_int b in
    let c = Char.to_int c in
    let d = Char.to_int d in
    Char.of_int_exn ((a + b + c + d + 2) lsr 2)
  ;;

  let subsample_h2 ~src ~dst ~row =
    let w = Plane.width dst in
    for col = 0 to w - 1 do
      Plane.(dst.![col, row] <- avg2 src.![col * 2, row] src.![(col * 2) + 1, row])
    done
  ;;

  let supersample_h2 ~src ~dst ~row =
    let w = Plane.width src in
    for col = 0 to w - 2 do
      Plane.(dst.![col * 2, row] <- src.![col, row]);
      Plane.(dst.![(col * 2) + 1, row] <- avg2 src.![col, row] src.![col + 1, row])
    done;
    Plane.(dst.![(w * 2) - 2, row] <- src.![w - 1, row]);
    Plane.(dst.![(w * 2) - 1, row] <- src.![w - 1, row])
  ;;

  let assert_is_444 (yuv : Yuv.t) =
    let wy, hy = Plane.width yuv.y, Plane.height yuv.y in
    let wu, hu = Plane.width yuv.u, Plane.height yuv.u in
    let wv, hv = Plane.width yuv.v, Plane.height yuv.v in
    if not (wy = wu && wu = wv && hy = hu && hu = hv)
    then
      raise_s
        [%message
          "Expecting a 4:4:4 frame"
            (wy, hy : int * int)
            (wu, hu : int * int)
            (wv, hv : int * int)]
  ;;

  let assert_is_422 (yuv : Yuv.t) =
    let wy, hy = Plane.width yuv.y, Plane.height yuv.y in
    let wu, hu = Plane.width yuv.u, Plane.height yuv.u in
    let wv, hv = Plane.width yuv.v, Plane.height yuv.v in
    if not (wy = wu * 2 && wu = wv && hy = hu && hu = hv)
    then
      raise_s
        [%message
          "Expecting a 4:2:2 frame"
            (wy, hy : int * int)
            (wu, hu : int * int)
            (wv, hv : int * int)]
  ;;

  let assert_is_420 (yuv : Yuv.t) =
    let wy, hy = Plane.width yuv.y, Plane.height yuv.y in
    let wu, hu = Plane.width yuv.u, Plane.height yuv.u in
    let wv, hv = Plane.width yuv.v, Plane.height yuv.v in
    if not (wy = wu * 2 && wu = wv && hy = hu * 2 && hu = hv)
    then
      raise_s
        [%message
          "Expecting a 4:2:0 frame"
            (wy, hy : int * int)
            (wu, hu : int * int)
            (wv, hv : int * int)]
  ;;

  let convert_to_422 ~(src : Yuv.t) ~(dst : Yuv.t) =
    assert_is_444 src;
    assert_is_422 dst;
    let height = Plane.height src.y in
    Plane.blit ~src:src.y ~dst:dst.y;
    for row = 0 to height - 1 do
      subsample_h2 ~src:src.u ~dst:dst.u ~row;
      subsample_h2 ~src:src.v ~dst:dst.v ~row
    done
  ;;

  let to_422 (src : Yuv.t) =
    let dst = Yuv.create_422 ~width:(Plane.width src.y) ~height:(Plane.height src.y) in
    convert_to_422 ~src ~dst;
    dst
  ;;

  let convert_from_422 ~(src : Yuv.t) ~(dst : Yuv.t) =
    assert_is_422 src;
    assert_is_444 dst;
    let height = Plane.height src.y in
    Plane.blit ~src:src.y ~dst:dst.y;
    for row = 0 to height - 1 do
      supersample_h2 ~src:src.u ~dst:dst.u ~row;
      supersample_h2 ~src:src.v ~dst:dst.v ~row
    done
  ;;

  let of_422 (src : Yuv.t) =
    let dst = Yuv.create_444 ~width:(Plane.width src.y) ~height:(Plane.height src.y) in
    convert_from_422 ~src ~dst;
    dst
  ;;

  let subsample_hv2 ~src ~dst ~row =
    let w = Plane.width dst in
    for col = 0 to w - 1 do
      Plane.(
        dst.![col, row]
          <- avg4
               src.![col * 2, row * 2]
               src.![(col * 2) + 1, row * 2]
               src.![col * 2, (row * 2) + 1]
               src.![(col * 2) + 1, (row * 2) + 1])
    done
  ;;

  let supersample_hv2 ~src ~dst ~row =
    let w = Plane.width src in
    let h = Plane.height src in
    let row1 = row in
    let row2 = min (h - 1) (row + 1) in
    for col = 0 to w - 2 do
      let a = Plane.(src.![col, row1]) in
      let b = Plane.(src.![col + 1, row1]) in
      let c = Plane.(src.![col, row2]) in
      let d = Plane.(src.![col + 1, row2]) in
      Plane.(dst.![col * 2, row * 2] <- a);
      Plane.(dst.![(col * 2) + 1, row * 2] <- avg2 a b);
      Plane.(dst.![col * 2, (row * 2) + 1] <- avg2 a c);
      Plane.(dst.![(col * 2) + 1, (row * 2) + 1] <- avg4 a b c d)
    done;
    let a = Plane.(src.![w - 1, row1]) in
    let b = Plane.(src.![w - 1, row2]) in
    Plane.(dst.![(w * 2) - 2, row * 2] <- a);
    Plane.(dst.![(w * 2) - 1, row * 2] <- a);
    Plane.(dst.![(w * 2) - 2, (row * 2) + 1] <- avg2 a b);
    Plane.(dst.![(w * 2) - 1, (row * 2) + 1] <- avg2 a b)
  ;;

  let convert_to_420 ~(src : Yuv.t) ~(dst : Yuv.t) =
    assert_is_420 dst;
    assert_is_444 src;
    let height = Plane.height src.y in
    Plane.blit ~src:src.y ~dst:dst.y;
    for row = 0 to (height / 2) - 1 do
      subsample_hv2 ~src:src.u ~dst:dst.u ~row;
      subsample_hv2 ~src:src.v ~dst:dst.v ~row
    done
  ;;

  let to_420 (src : Yuv.t) =
    let dst = Yuv.create_420 ~width:(Plane.width src.y) ~height:(Plane.height src.y) in
    convert_to_420 ~src ~dst;
    dst
  ;;

  let convert_from_420 ~(src : Yuv.t) ~(dst : Yuv.t) =
    assert_is_420 src;
    assert_is_444 dst;
    let height = Plane.height src.y in
    Plane.blit ~src:src.y ~dst:dst.y;
    for row = 0 to (height / 2) - 1 do
      supersample_hv2 ~src:src.u ~dst:dst.u ~row;
      supersample_hv2 ~src:src.v ~dst:dst.v ~row
    done
  ;;

  let of_420 (src : Yuv.t) =
    let dst = Yuv.create_444 ~width:(Plane.width src.y) ~height:(Plane.height src.y) in
    convert_from_420 ~src ~dst;
    dst
  ;;

  let%expect_test "444<->422" =
    let f = Yuv.create_444 ~width:4 ~height:4 in
    for row = 0 to 3 do
      for col = 0 to 3 do
        Plane.(f.y.![col, row] <- Char.of_int_exn (row + (col * 10)));
        Plane.(f.u.![col, row] <- Char.of_int_exn (50 + row + (col * 10)));
        Plane.(f.v.![col, row] <- Char.of_int_exn (100 + row + (col * 10)))
      done
    done;
    Yuv.dump_yuv f;
    [%expect
      {|
      0  10  20  30
      1  11  21  31
      2  12  22  32
      3  13  23  33
     50  60  70  80
     51  61  71  81
     52  62  72  82
     53  63  73  83
    100 110 120 130
    101 111 121 131
    102 112 122 132
    103 113 123 133 |}];
    let f422 = to_422 f in
    Yuv.dump_yuv f422;
    [%expect
      {|
        0  10  20  30
        1  11  21  31
        2  12  22  32
        3  13  23  33
       55  75
       56  76
       57  77
       58  78
      105 125
      106 126
      107 127
      108 128 |}];
    let f444 = of_422 f422 in
    Yuv.dump_yuv f444;
    [%expect
      {|
        0  10  20  30
        1  11  21  31
        2  12  22  32
        3  13  23  33
       55  65  75  75
       56  66  76  76
       57  67  77  77
       58  68  78  78
      105 115 125 125
      106 116 126 126
      107 117 127 127
      108 118 128 128 |}]
  ;;

  let%expect_test "444<->420" =
    let f = Yuv.create_444 ~width:4 ~height:4 in
    for row = 0 to 3 do
      for col = 0 to 3 do
        Plane.(f.y.![col, row] <- Char.of_int_exn (row + (col * 10)));
        Plane.(f.u.![col, row] <- Char.of_int_exn (50 + row + (col * 10)));
        Plane.(f.v.![col, row] <- Char.of_int_exn (100 + row + (col * 10)))
      done
    done;
    Yuv.dump_yuv f;
    [%expect
      {|
      0  10  20  30
      1  11  21  31
      2  12  22  32
      3  13  23  33
     50  60  70  80
     51  61  71  81
     52  62  72  82
     53  63  73  83
    100 110 120 130
    101 111 121 131
    102 112 122 132
    103 113 123 133 |}];
    let f420 = to_420 f in
    Yuv.dump_yuv f420;
    [%expect
      {|
        0  10  20  30
        1  11  21  31
        2  12  22  32
        3  13  23  33
       56  76
       58  78
      106 126
      108 128 |}];
    let f444 = of_420 f420 in
    Yuv.dump_yuv f444;
    [%expect
      {|
        0  10  20  30
        1  11  21  31
        2  12  22  32
        3  13  23  33
       56  66  76  76
       57  67  77  77
       58  68  78  78
       58  68  78  78
      106 116 126 126
      107 117 127 127
      108 118 128 128
      108 118 128 128 |}]
  ;;
end

module Packed_422 = struct
  type packed_yuv_format = int array

  let yuy2 = [| 0; 1; 3 |]
  let uyvy = [| 1; 0; 2 |]
  let yvyu = [| 0; 3; 1 |]

  let convert_to_planar fmt ~(src : Plane.t) ~(dst : Yuv.t) =
    (* width and height of luma plane *)
    let w = Plane.width src / 2 in
    let h = Plane.height src in
    let yo, uo, vo = fmt.(0), fmt.(1), fmt.(2) in
    for row = 0 to h - 1 do
      for col = 0 to (w / 2) - 1 do
        Plane.(dst.y.![col * 2, row] <- src.![(col * 4) + yo, row]);
        Plane.(dst.y.![(col * 2) + 1, row] <- src.![(col * 4) + yo + 2, row]);
        Plane.(dst.u.![col, row] <- src.![(col * 4) + uo, row]);
        Plane.(dst.v.![col, row] <- src.![(col * 4) + vo, row])
      done
    done
  ;;

  let to_planar fmt (src : Plane.t) =
    let w = Plane.width src / 2 in
    let h = Plane.height src in
    let dst = Yuv.create_422 ~width:w ~height:h in
    convert_to_planar fmt ~src ~dst;
    dst
  ;;

  let convert_from_planar fmt ~(src : Yuv.t) ~dst =
    (* width and height of luma plane *)
    let w = Plane.width src.y in
    let h = Plane.height src.y in
    let yo, uo, vo = fmt.(0), fmt.(1), fmt.(2) in
    for row = 0 to h - 1 do
      for col = 0 to (w / 2) - 1 do
        Plane.(dst.![(col * 4) + yo, row] <- src.y.![col * 2, row]);
        Plane.(dst.![(col * 4) + yo + 2, row] <- src.y.![(col * 2) + 1, row]);
        Plane.(dst.![(col * 4) + uo, row] <- src.u.![col, row]);
        Plane.(dst.![(col * 4) + vo, row] <- src.v.![col, row])
      done
    done
  ;;

  let of_planar fmt (src : Yuv.t) =
    let w = Plane.width src.y in
    let h = Plane.height src.y in
    let dst = Plane.create ~width:(w * 2) ~height:h in
    convert_from_planar fmt ~src ~dst;
    dst
  ;;

  let%expect_test "planar <-> packed" =
    let f = Yuv.create_422 ~width:4 ~height:4 in
    for row = 0 to 3 do
      for col = 0 to 3 do
        Plane.(f.y.![col, row] <- Char.of_int_exn (row + (col * 10)))
      done
    done;
    for row = 0 to 3 do
      for col = 0 to 1 do
        Plane.(f.u.![col, row] <- Char.of_int_exn (50 + row + (col * 10)));
        Plane.(f.v.![col, row] <- Char.of_int_exn (100 + row + (col * 10)))
      done
    done;
    let packed = of_planar yuy2 f in
    let planar = to_planar yuy2 packed in
    Yuv.dump_yuv f;
    Yuv.dump packed;
    Yuv.dump_yuv planar;
    [%expect
      {|
        0  10  20  30
        1  11  21  31
        2  12  22  32
        3  13  23  33
       50  60
       51  61
       52  62
       53  63
      100 110
      101 111
      102 112
      103 113
        0  50  10 100  20  60  30 110
        1  51  11 101  21  61  31 111
        2  52  12 102  22  62  32 112
        3  53  13 103  23  63  33 113
        0  10  20  30
        1  11  21  31
        2  12  22  32
        3  13  23  33
       50  60
       51  61
       52  62
       53  63
      100 110
      101 111
      102 112
      103 113 |}]
  ;;
end

module Format = struct
  module Packed = struct
    type t =
      | YUY2
      | UYVY
      | YVYU
    [@@deriving sexp_of]

    let get_format = function
      | YUY2 -> Packed_422.yuy2
      | UYVY -> Packed_422.uyvy
      | YVYU -> Packed_422.yvyu
    ;;

    let create { Size.width; height } = Plane.create ~width:(width * 2) ~height
    let input file p = Plane.input p file
    let output file p = Plane.output p file
  end

  module Planar = struct
    type t =
      | C420
      | C422
      | C444
    [@@deriving sexp_of]

    let create { Size.width; height } ~fmt =
      let cwidth =
        match fmt with
        | C420 | C422 -> width / 2
        | C444 -> width
      in
      let cheight =
        match fmt with
        | C420 -> height / 2
        | C422 | C444 -> height
      in
      { Yuv.y = Plane.create ~width ~height
      ; u = Plane.create ~width:cwidth ~height:cheight
      ; v = Plane.create ~width:cwidth ~height:cheight
      }
    ;;

    let input file { Yuv.y; u; v } =
      Plane.input y file;
      Plane.input u file;
      Plane.input v file
    ;;

    let output file { Yuv.y; u; v } =
      Plane.output y file;
      Plane.output u file;
      Plane.output v file
    ;;
  end

  type ('packed, 'planar) t =
    | Packed of 'packed
    | Planar of 'planar
  [@@deriving sexp_of]

  type packed_or_planar = (Packed.t, Planar.t) t [@@deriving sexp_of]

  let arg_type =
    Command.Arg_type.create (fun s ->
        match String.uppercase s with
        | "420" -> Planar Planar.C420
        | "422" -> Planar Planar.C422
        | "444" -> Planar Planar.C444
        | "YUY2" -> Packed Packed.YUY2
        | "UYVY" -> Packed Packed.UYVY
        | "YVYU" -> Packed Packed.YVYU
        | _ -> raise_s [%message "Invalid YUV format"])
  ;;

  let create size fmt =
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
      Planar_444.assert_is_420 src;
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
end

type frame =
  { format : Format.packed_or_planar
  ; size : Size.t
  }
[@@deriving sexp_of]

module Range = struct
  type t =
    { start : int
    ; end_ : int
    }
  [@@deriving sexp_of]

  let arg_type =
    Command.Arg_type.create (fun s ->
        try
          match String.split_on_chars ~on:[ 'x'; ','; '-' ] s with
          | [ start ] -> { start = Int.of_string start; end_ = Int.of_string start }
          | [ ""; end_ ] -> { start = 0; end_ = Int.of_string end_ }
          | [ start; end_ ] -> { start = Int.of_string start; end_ = Int.of_string end_ }
          | _ -> raise Caml.Not_found
        with
        | _ -> raise_s [%message "Invalid frame size specified" (s : string)])
  ;;
end

module Offset = struct
  type t =
    { x_off : int
    ; y_off : int
    }
  [@@deriving sexp_of]

  let arg_type =
    Command.Arg_type.create (fun s ->
        try
          match String.split_on_chars ~on:[ 'x'; ','; '-' ] s with
          | [ x_off; y_off ] ->
            { x_off = Int.of_string x_off; y_off = Int.of_string y_off }
          | _ -> raise Caml.Not_found
        with
        | _ -> raise_s [%message "Invalid frame size specified" (s : string)])
  ;;
end

type t =
  { in_file : string
  ; out_file : string
  ; src : frame
  ; src_offset : Offset.t
  ; dst : frame
  ; frames : Range.t
  }
[@@deriving sexp_of]

let default_format = Format.Planar Format.Planar.C420

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
      flag "-format" (optional_with_default default_format Format.arg_type) ~doc:""
    and format_out = flag "-out-format" (optional Format.arg_type) ~doc:""
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
  let input_pipe = Format.create t.src.size t.src.format in
  let input_frame = Yuv.create_444 ~width:t.src.size.width ~height:t.src.size.height in
  let output_pipe = Format.create t.dst.size t.dst.format in
  let output_frame = Yuv.create_444 ~width:t.dst.size.width ~height:t.dst.size.height in
  with_in_file t.in_file ~f:(fun in_file ->
      with_out_file t.out_file ~f:(fun out_file ->
          With_return.with_return (fun { return } ->
              for _ = 0 to t.frames.start - 1 do
                if not (Format.input in_file input_frame input_pipe) then return ()
              done;
              for _ = 0 to t.frames.end_ - t.frames.start do
                if Format.input in_file input_frame input_pipe
                then (
                  Yuv.crop
                    ~x_pos:t.src_offset.x_off
                    ~y_pos:t.src_offset.y_off
                    ~src:input_frame
                    ~dst:output_frame;
                  Format.output out_file output_frame output_pipe)
                else return ()
              done)))
;;
