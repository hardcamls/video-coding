open! Core
open! Hardcaml_video_common

let crop ~x_pos ~y_pos ~(src : Oconv.Yuv.t) ~(dst : Oconv.Yuv.t) =
  let width = Plane.width dst.y in
  let height = Plane.height dst.y in
  for row = 0 to width - 1 do
    for col = 0 to height - 1 do
      Plane.(dst.y.![row, col] <- src.y.![row + x_pos, col + y_pos]);
      Plane.(dst.u.![row, col] <- src.u.![row + x_pos, col + y_pos]);
      Plane.(dst.v.![row, col] <- src.v.![row + x_pos, col + y_pos])
    done
  done
;;
