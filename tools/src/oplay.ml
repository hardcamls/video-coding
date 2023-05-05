open Core
open Tsdl
open Printf

module Yuv_format = struct
  type t =
    | YUY2
    | UYVY
    | YVYU
    | C420
    | YV12
    | C422
  [@@deriving equal]

  let to_format = function
    | YUY2 -> Sdl.Pixel.format_yuy2
    | UYVY -> Sdl.Pixel.format_uyvy
    | YVYU -> Sdl.Pixel.format_yvyu
    | C420 -> Sdl.Pixel.format_iyuv
    | YV12 -> Sdl.Pixel.format_yv12
    | C422 -> Sdl.Pixel.format_yuy2
  ;;

  let of_string s =
    match String.uppercase s with
    | "YUY2" -> YUY2
    | "UYVY" -> UYVY
    | "YVYU" -> YVYU
    | "420" -> C420
    | "YV12" -> YV12
    | "422" -> C422
    | _ -> raise_s [%message "Invalid yuv format"]
  ;;

  let total_bytes width height = function
    | YUY2 | UYVY | YVYU | C422 -> width * height * 2
    | C420 | YV12 -> width * height * 3 / 2
  ;;

  let stride width = function
    | YUY2 | UYVY | YVYU | C422 -> width * 2
    | C420 | YV12 -> width
  ;;
end

module Diff_mode = struct
  type t =
    | NoDiff
    | OtherFile
    | ExactDiff
    | Diff

  let next = function
    | NoDiff -> OtherFile
    | OtherFile -> ExactDiff
    | ExactDiff -> Diff
    | Diff -> NoDiff
  ;;
end

module Plane_mode = struct
  type t =
    | All
    | Y
    | U
    | V

  let next = function
    | All -> Y
    | Y -> U
    | U -> V
    | V -> All
  ;;
end

type t =
  { in_file : string option
  ; diff_file : string option
  ; size : Size.t
  ; size_out : Size.t
  ; format : Yuv_format.t
  ; mutable framerate : int
  ; mutable fullscreen : bool
  ; mutable grid : bool
  ; mutable diff_mode : Diff_mode.t
  ; mutable show_plane : Plane_mode.t
  ; verbose : bool
  }

type ba = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
type ba2 = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array2.t

module File_io = struct
  module Unix = Core_unix

  type file =
    | UnixFile of Unix.File_descr.t * ba2 * int
    | CamlFile of In_channel.t * ba

  let frame_size w h fmt = Yuv_format.total_bytes w h fmt

  let open_unix t name =
    (* get length of file, and thus number of frames *)
    let length = (Unix.stat name).st_size in
    let size = frame_size t.size.width t.size.height t.format in
    let num_frames = Int64.to_int_exn Int64.(length / Int64.of_int size) in
    (* open file *)
    let f = In_channel.create ~binary:true name in
    let f = Unix.descr_of_in_channel f in
    (* map file *)
    let m =
      Unix.map_file
        f
        ~pos:0L
        Bigarray.int8_unsigned
        Bigarray.c_layout
        ~shared:false
        [| num_frames; size |]
      |> Bigarray.array2_of_genarray
    in
    UnixFile (f, m, max 1 num_frames)
  ;;

  let open_stdin t =
    let size = frame_size t.size.width t.size.height t.format in
    let ba = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout size in
    CamlFile (In_channel.stdin, ba)
  ;;

  (* open file, or redirect from stdin *)
  let open_file_or_stdin t name =
    try
      match name with
      | None | Some "-" -> open_stdin t
      | Some name -> open_unix t name
    with
    | _ -> raise_s [%message "Failed to open file" (name : string option)]
  ;;

  let open_file_or_none t name =
    try Option.map name ~f:(open_unix t) with
    | _ -> raise_s [%message "Failed to open file" (name : string option)]
  ;;

  let[@cold] raise_end_of_image () = raise_s [%message "End of image while reading file"]

  let read_file file frame_no =
    match file with
    | UnixFile (_, b, s) ->
      let frame_no = frame_no mod s in
      Bigarray.Array2.slice_left b frame_no
    | CamlFile (f, b) ->
      let size = Bigarray.Array1.dim b in
      let rec rd n =
        if n <> size
        then (
          b.{n}
            <- (match In_channel.input_byte f with
               | Some x -> x
               | None -> raise_end_of_image ());
          rd (n + 1))
      in
      rd 0;
      b
  ;;

  let read_file_opt file frame_no =
    match file with
    | None -> None
    | Some file -> Some (read_file file frame_no)
  ;;
end

module Transform = struct
  let get_buffer (f : ba) (t : ba option ref) =
    match !t with
    | Some b -> b
    | None ->
      let size = Bigarray.Array1.dim f in
      let b = Bigarray.(Array1.create int8_unsigned c_layout size) in
      t := Some b;
      b
  ;;

  let rec loop x lim step f =
    if x <= lim
    then (
      f x;
      loop (x + step) lim step f)
  ;;

  let off_pixel_420_planar t flip =
    let w = t.size.width in
    let h = t.size.height in
    let ou = w * h in
    let ov = ou + (w * h / 4) in
    let ou, ov = if flip then ov, ou else ou, ov in
    ( (fun x y -> x + (y * w))
    , (fun x y -> ou + (x / 2) + (y / 2 * (w / 2)))
    , fun x y -> ov + (x / 2) + (y / 2 * (w / 2)) )
  ;;

  let off_pixel_422_packed t oy ou ov =
    let w = t.size.width in
    ( (fun x y -> (x * 2) + (y * w * 2) + oy)
    , (fun x y -> (x / 2 * 4) + (y * w * 2) + ou)
    , fun x y -> (x / 2 * 4) + (y * w * 2) + ov )
  ;;

  let off_pixel t =
    match t.format with
    | YUY2 -> off_pixel_422_packed t 0 1 3
    | UYVY -> off_pixel_422_packed t 1 0 2
    | YVYU -> off_pixel_422_packed t 0 3 1
    | YV12 -> off_pixel_420_planar t true
    | C420 -> off_pixel_420_planar t false
    | C422 -> off_pixel_422_packed t 0 1 3
  ;;

  let copy422_to_yuy2 =
    let b = ref None in
    fun t f ->
      let b = get_buffer f b in
      let width, height = t.size.width, t.size.height in
      for y = 0 to height - 1 do
        let h = width * y in
        for x = 0 to width - 1 do
          let p = f.{h + x} in
          b.{(h * 2) + (x * 2)} <- p
        done
      done;
      let c = width * height in
      for y = 0 to height - 1 do
        let h = width / 2 * y in
        for x = 0 to (width / 2) - 1 do
          b.{(h * 4) + (x * 4) + 1} <- f.{c + h + x}
        done
      done;
      let c = c + (width * height / 2) in
      for y = 0 to height - 1 do
        let h = width / 2 * y in
        for x = 0 to (width / 2) - 1 do
          b.{(h * 4) + (x * 4) + 3} <- f.{c + h + x}
        done
      done;
      b
  ;;

  let draw_plane =
    let b = ref None in
    fun t f ->
      let b = get_buffer f b in
      let oy, ou, ov = off_pixel t in
      let loop f =
        loop 0 (t.size.width - 1) 1 (fun x ->
            loop 0 (t.size.height - 1) 1 (fun y ->
                let oy, ou, ov = oy x y, ou x y, ov x y in
                let vy, vu, vv = f oy ou ov in
                b.{oy} <- vy;
                b.{ou} <- vu;
                b.{ov} <- vv))
      in
      (match t.show_plane with
      | All -> ()
      | Y -> loop (fun oy _ou _ov -> f.{oy}, 128, 128)
      | U -> loop (fun _oy ou _ov -> 128, f.{ou}, 128)
      | V -> loop (fun _oy _ou ov -> 128, 128, f.{ov}));
      b
  ;;

  let draw_diff =
    let b = ref None in
    fun t f1 (f2 : ba) ->
      let b = get_buffer f1 b in
      let oy, ou, ov = off_pixel t in
      loop 0 (t.size.width - 1) 1 (fun x ->
          loop 0 (t.size.height - 1) 1 (fun y ->
              let oy, ou, ov = oy x y, ou x y, ov x y in
              b.{oy} <- abs (f1.{oy} - f2.{oy});
              b.{ou} <- ((f1.{oy} - f2.{oy}) / 2) + 128;
              b.{ov} <- ((f1.{oy} - f2.{oy}) / 2) + 128));
      b
  ;;

  let draw_exact_diff =
    let b = ref None in
    fun t f1 (f2 : ba) ->
      let size = Bigarray.Array1.dim f1 in
      let b = get_buffer f1 b in
      for i = 0 to size - 1 do
        b.{i} <- (if f1.{i} <> f2.{i} then 255 else 0)
      done;
      let oy, ou, ov = off_pixel t in
      loop 0 (t.size.width - 1) 1 (fun x ->
          loop 0 (t.size.height - 1) 1 (fun y ->
              let oy, ou, ov = oy x y, ou x y, ov x y in
              b.{oy}
                <- (if f1.{oy} <> f2.{oy} || f1.{ou} <> f2.{ou} || f1.{ov} <> f2.{ov}
                   then 255
                   else 0);
              b.{ou} <- 128;
              b.{ov} <- 128));
      b
  ;;

  let draw_grid =
    let b = ref None in
    fun t f ->
      let b = get_buffer f b in
      let oy, ou, ov = off_pixel t in
      Bigarray.Array1.blit f b;
      loop 16 (t.size.width - 1) 16 (fun x ->
          loop 0 (t.size.height - 1) 1 (fun y ->
              b.{oy x y} <- 255;
              b.{ou x y} <- 128;
              b.{ov x y} <- 128));
      loop 16 (t.size.height - 1) 16 (fun y ->
          loop 0 (t.size.width - 1) 1 (fun x ->
              b.{oy x y} <- 255;
              b.{ou x y} <- 128;
              b.{ov x y} <- 128));
      b
  ;;

  let run t f1 f2 =
    (* convert 422 to yuy2 *)
    let f1, f2 =
      match Yuv_format.equal t.format C422, f2 with
      | true, Some f2 -> copy422_to_yuy2 t f1, Some (copy422_to_yuy2 t f2)
      | true, None -> copy422_to_yuy2 t f1, None
      | _ -> f1, f2
    in
    (* select plane *)
    let f1, f2 =
      match t.show_plane, f2 with
      | All, _ -> f1, f2
      | _, Some f2 -> draw_plane t f1, Some (draw_plane t f2)
      | _, None -> draw_plane t f1, None
    in
    (* difference *)
    let f =
      match f2 with
      | None -> f1
      | Some f2 ->
        (match t.diff_mode with
        | NoDiff -> f1
        | OtherFile -> f2
        | Diff -> draw_diff t f1 f2
        | ExactDiff -> draw_exact_diff t f1 f2)
    in
    (* grid *)
    let f = if t.grid then draw_grid t f else f in
    f
  ;;
end

(* app main loop *)
let main t =
  let module K = Sdl.K in
  (* simple resource management - undo what was done *)
  let cleanup = ref [] in
  let do_cleanup () = List.iter ~f:(fun f -> f ()) !cleanup in
  let create_resource f c =
    match f with
    | Result.Ok a ->
      cleanup := (fun () -> c a) :: !cleanup;
      a
    | Result.Error (`Msg _) ->
      do_cleanup ();
      failwith "resource allocation"
  in
  try
    (* sdl setup *)
    let () = create_resource (Sdl.init Sdl.Init.video) Sdl.quit in
    let window_flags () =
      Sdl.Window.(if t.fullscreen then fullscreen_desktop else windowed)
    in
    let window =
      create_resource
        (Sdl.create_window
           ~w:t.size_out.width
           ~h:t.size_out.height
           "OCaml YUV Player"
           (window_flags ()))
        Sdl.destroy_window
    in
    let renderer = create_resource (Sdl.create_renderer window) Sdl.destroy_renderer in
    let _ = Sdl.render_set_logical_size renderer t.size.width t.size.height in
    let _ = Sdl.set_hint Sdl.Hint.render_scale_quality "linear" in
    let _ = Sdl.render_clear renderer in
    let display =
      create_resource
        (Sdl.create_texture
           renderer
           (Yuv_format.to_format t.format)
           Sdl.Texture.(access_streaming)
           ~w:t.size.width
           ~h:t.size.height)
        Sdl.destroy_texture
    in
    (* open file(s) *)
    let fin = File_io.open_file_or_stdin t t.in_file in
    let fref = File_io.open_file_or_none t t.diff_file in
    let total_frames =
      match fin with
      | File_io.UnixFile (_, _, n) -> n
      | _ -> 1
    in
    let set_frame_no, get_frame_no =
      let frameno = ref 0 in
      let rec set n =
        if n < 0 then set (n + total_frames) else frameno := n mod total_frames
      in
      set, fun () -> !frameno
    in
    (* key presses *)
    let finish = ref false in
    let redraw = ref false in
    let play = ref false in
    let key_presses key =
      if key = K.escape
      then finish := true
      else if key = K.tab
      then (
        t.fullscreen <- not t.fullscreen;
        ignore (Sdl.set_window_fullscreen window (window_flags ()));
        redraw := true;
        ignore (Sdl.render_clear renderer))
      else if key = K.return || key = K.space
      then play := not !play
      else if key = K.right
      then (
        play := false;
        set_frame_no (get_frame_no () + 1);
        redraw := true)
      else if key = K.left
      then (
        play := false;
        set_frame_no (get_frame_no () - 1);
        redraw := true)
      else if key = K.home || key = K.k0
      then (
        play := false;
        set_frame_no 0;
        redraw := true)
      else if key = K.kend
      then (
        play := false;
        set_frame_no (total_frames - 1);
        redraw := true)
      else if key = K.g
      then (
        t.grid <- not t.grid;
        redraw := true)
      else if key = K.d
      then (
        t.diff_mode <- Diff_mode.next t.diff_mode;
        redraw := true)
      else if key = K.y
      then (
        t.show_plane <- Plane_mode.next t.show_plane;
        redraw := true)
      else (
        let num =
          if key = K.k1
          then 1
          else if key = K.k2
          then 2
          else if key = K.k3
          then 3
          else if key = K.k4
          then 4
          else if key = K.k5
          then 5
          else if key = K.k6
          then 6
          else if key = K.k7
          then 7
          else if key = K.k8
          then 8
          else if key = K.k9
          then 9
          else -1
        in
        if num <> -1
        then (
          let num = float_of_int num in
          let num = int_of_float ((float_of_int total_frames *. (num /. 10.0)) +. 0.5) in
          play := false;
          set_frame_no num;
          redraw := true))
    in
    let display_frame frame_no =
      if !redraw
      then (
        let b1 = File_io.read_file fin frame_no in
        let b2 = File_io.read_file_opt fref frame_no in
        let b = Transform.run t b1 b2 in
        ignore
          (Sdl.update_texture display None b (Yuv_format.stride t.size.width t.format));
        ignore (Sdl.render_copy renderer display);
        ignore (Sdl.render_present renderer);
        redraw := false;
        Sdl.set_window_title window (sprintf "[%i] oplay" frame_no))
    in
    (* playout at given rate *)
    let ftimems () = Int32.to_float (Sdl.get_ticks ()) in
    let lasttime = ref (ftimems ()) in
    let playout =
      let lapse = 1000.0 /. float_of_int t.framerate in
      fun () ->
        if !play
        then (
          let newtime = ftimems () in
          if Float.(newtime - !lasttime >= lapse)
          then (
            lasttime := newtime;
            redraw := true;
            set_frame_no (get_frame_no () + 1)))
    in
    (* polling loop *)
    let event = Sdl.Event.create () in
    let rec poll' () =
      Sdl.delay 1l;
      (* note; this stops the CPU going @ 100% *)
      (* playback timer *)
      playout ();
      (* draw frame *)
      display_frame (get_frame_no ());
      (* poll events *)
      if not !finish
      then (
        while Sdl.poll_event (Some event) do
          let open Sdl.Event in
          let typ = get event typ in
          if typ = quit
          then (* quit *)
            finish := true
          else if typ = key_down
          then (* key press *)
            key_presses (get event keyboard_keycode)
          else if typ = window_event
          then (
            (* window event *)
            let wev = get event window_event_id in
            if wev = window_event_exposed then redraw := true else ())
          else ()
        done;
        poll' ())
    in
    poll' ()
  with
  | Failure x ->
    do_cleanup ();
    printf "FAILURE: %s\n%!" x;
    exit (-1)
;;

(* command line *)
let readme () =
  {|
 
Raw YUV frame video player.

Frame sizes can be specified as 'WidthxHeight' or by commonly used names.

Various packed and unpacked formats are supported with 4:2:0 or 4:2:2 sampling modes.

A reference frame may be suppplied and pixel differences shown.  Individual
luma/colour planes can be shown seperately.

=== Controls ===

  escape   quit
  space    play/stop
  return   play/stop
  left     previous frame 
  right    next frame
  home     first frame
  end      last frame
  0..9     seek % into file
  tab      toggle fullscreen
  y        toggle displayed plane 
  d        toggle differencfe mode 
  g        overlay 16x15 grid
  |}
;;

let arg =
  [%map_open.Command
    let size = anon ("SIZE" %: Size.arg_type)
    and in_file = anon (maybe ("IN-FILE-IN" %: string))
    and diff_file = flag "-diff" (optional string) ~doc:" file to diff against"
    and size_out = flag "-s" (optional Size.arg_type) ~doc:" display size"
    and format =
      flag
        "-f"
        (optional_with_default Yuv_format.C420 (Arg_type.create Yuv_format.of_string))
        ~doc:" YUV format"
    and framerate = flag "-r" (optional_with_default 30 int) ~doc:" frame rate"
    and fullscreen = flag "-d" no_arg ~doc:" start in fullscreen mode"
    and verbose = flag "-v" no_arg ~doc:" verbose" in
    { in_file
    ; diff_file
    ; size
    ; size_out = Option.value size_out ~default:size
    ; format
    ; framerate
    ; fullscreen
    ; grid = false
    ; diff_mode = Diff_mode.NoDiff
    ; show_plane = Plane_mode.All
    ; verbose
    }]
;;
