open! Core
open Hardcaml_video_common

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
  Yuv.For_testing.dump_yuv f;
  Yuv.For_testing.dump packed;
  Yuv.For_testing.dump_yuv planar;
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
