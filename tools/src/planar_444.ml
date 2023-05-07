open! Core
open Hardcaml_video_common

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

let convert_to_422 ~(src : Yuv.t) ~(dst : Yuv.t) =
  Yuv.assert_is_444 src;
  Yuv.assert_is_422 dst;
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
  Yuv.assert_is_422 src;
  Yuv.assert_is_444 dst;
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
  Yuv.assert_is_420 dst;
  Yuv.assert_is_444 src;
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
  Yuv.assert_is_420 src;
  Yuv.assert_is_444 dst;
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
  Yuv.For_testing.dump_yuv f;
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
  Yuv.For_testing.dump_yuv f422;
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
  Yuv.For_testing.dump_yuv f444;
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
  Yuv.For_testing.dump_yuv f;
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
  Yuv.For_testing.dump_yuv f420;
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
  Yuv.For_testing.dump_yuv f444;
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
