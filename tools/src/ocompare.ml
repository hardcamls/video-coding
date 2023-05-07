(* Max difference and PSNR *)

open! Core
open Hardcaml_video_common

let diff a b = Int.abs (Char.to_int a - Char.to_int b)

let max_difference (f1 : Plane.t) (f2 : Plane.t) =
  assert (Plane.width f1 = Plane.width f2);
  assert (Plane.height f1 = Plane.height f2);
  let max_diff = ref 0 in
  for row = 0 to Plane.height f1 - 1 do
    for col = 0 to Plane.width f1 - 1 do
      max_diff := max !max_diff Plane.(diff f1.![col, row] f2.![col, row])
    done
  done;
  !max_diff
;;

let total_difference (f1 : Plane.t) (f2 : Plane.t) =
  assert (Plane.width f1 = Plane.width f2);
  assert (Plane.height f1 = Plane.height f2);
  let acc = ref 0 in
  for row = 0 to Plane.height f1 - 1 do
    for col = 0 to Plane.width f1 - 1 do
      acc := !acc + Plane.(diff f1.![col, row] f2.![col, row])
    done
  done;
  !acc
;;

let mean_difference f1 f2 =
  let width = Float.of_int (Plane.width f1) in
  let height = Float.of_int (Plane.height f1) in
  Float.(of_int (total_difference f1 f2) / (width * height))
;;

let square_error (f1 : Plane.t) (f2 : Plane.t) =
  assert (Plane.width f1 = Plane.width f2);
  assert (Plane.height f1 = Plane.height f2);
  let acc = ref 0 in
  for row = 0 to Plane.height f1 - 1 do
    for col = 0 to Plane.width f1 - 1 do
      let diff = Plane.(diff f1.![col, row] f2.![col, row]) in
      acc := !acc + (diff * diff)
    done
  done;
  !acc
;;

let mean_square_error f1 f2 =
  let width = Float.of_int (Plane.width f1) in
  let height = Float.of_int (Plane.height f1) in
  Float.(of_int (square_error f1 f2) / (width * height))
;;

let psnr ?(r = 255.) (f1 : Plane.t) (f2 : Plane.t) =
  Float.(10. * log10 (r * r / mean_square_error f1 f2))
;;

let input file (size : Size.t) (fmt : Yuv_format.t) =
  let input_planar p =
    Yuv_format.Planar.input file p;
    p
  in
  match fmt with
  | Packed fmt ->
    let fmt = Yuv_format.Packed.get_format fmt in
    let p = Plane.create ~width:size.width ~height:size.height in
    Yuv_format.Packed.input file p;
    Packed_422.to_planar fmt p
  | Planar C420 -> Yuv.create_420 ~width:size.width ~height:size.height |> input_planar
  | Planar C422 -> Yuv.create_422 ~width:size.width ~height:size.height |> input_planar
  | Planar C444 -> Yuv.create_444 ~width:size.width ~height:size.height |> input_planar
;;

(* 
compare 
   [max-difference|mean-difference|mean-square-error|psnr]   
   [y|u|v|yuv]
*)

let command plane fn =
  Command.basic
    ~summary:""
    [%map_open.Command
      let f1 = anon ("FILE-1" %: string)
      and f2 = anon ("FILE-2" %: string)
      and size = anon ("SIZE" %: Size.arg_type)
      and fmt =
        flag
          "-format"
          (optional_with_default Yuv_format.(Planar Planar.C420) Yuv_format.arg_type)
          ~doc:""
      in
      fun () ->
        In_channel.with_file f1 ~f:(fun f1 ->
            In_channel.with_file f2 ~f:(fun f2 ->
                let f1 = input f1 size fmt in
                let f2 = input f2 size fmt in
                match plane with
                | `y -> fn f1.y f2.y
                | `u -> fn f1.u f2.u
                | `v -> fn f1.v f2.v
                | `yuv ->
                  fn f1.y f2.y;
                  fn f1.u f2.u;
                  fn f1.v f2.v))]
;;

let commands =
  let print_int f f1 f2 =
    let result = f f1 f2 in
    print_s [%sexp (result : int)]
  in
  let print_float f f1 f2 =
    let result = f f1 f2 in
    print_s [%sexp (result : float)]
  in
  Command.group
    ~summary:""
    (List.map
       [ "max-difference", print_int max_difference
       ; "mean-difference", print_float mean_difference
       ; "mean-square-error", print_float mean_square_error
       ; "psnr", print_float psnr
       ]
       ~f:(fun (n, f) ->
         ( n
         , Command.group
             ~summary:""
             (List.map
                [ "y", `y; "u", `u; "v", `v; "yuv", `yuv ]
                ~f:(fun (n, p) -> n, command p f)) )))
;;
