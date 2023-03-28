open Tsdl
open Printf

module YuvFormat = struct
  let packed =
    [ "YUY2", Sdl.Pixel.format_yuy2
    ; "UYVY", Sdl.Pixel.format_uyvy
    ; "YVYU", Sdl.Pixel.format_yvyu
    ; "422", Sdl.Pixel.format_yuy2 (* a bodge *)
    ]
  ;;

  let planar =
    [ "YV12", Sdl.Pixel.format_yv12
    ; "IYUV", Sdl.Pixel.format_iyuv
    ; "YUV", Sdl.Pixel.format_iyuv
    ; "420", Sdl.Pixel.format_iyuv
    ]
  ;;

  let formats = packed @ planar
  let find formats f = List.find (fun (_, f') -> Sdl.Pixel.eq f f') formats

  let is_planar f =
    try
      ignore (find planar f);
      true
    with
    | _ -> false
  ;;

  let is_packed f =
    try
      ignore (find packed f);
      true
    with
    | _ -> false
  ;;

  let string_of_format f =
    try fst (find formats f) with
    | _ -> failwith "invalid pixel format"
  ;;
end

(* command line parsing *)
module Arg = struct
  type diff_mode =
    | NoDiff
    | OtherFile
    | ExactDiff
    | Diff

  let next_diff_mode = function
    | NoDiff -> OtherFile
    | OtherFile -> ExactDiff
    | ExactDiff -> Diff
    | Diff -> NoDiff
  ;;

  type plane_mode =
    | All
    | Y
    | U
    | V

  let next_plane_mode = function
    | All -> Y
    | Y -> U
    | U -> V
    | V -> All
  ;;

  module Cfg = struct
    let in_file = ref ""
    let diff_file = ref ""
    let width = ref 0
    let height = ref 0
    let widtho = ref 0
    let heighto = ref 0
    let format = ref Sdl.Pixel.format_iyuv
    let auto422 = ref false
    let framerate = ref 30
    let fullscreen = ref false
    let grid = ref false
    let diff_mode = ref NoDiff
    let show_plane = ref All
    let verbose = ref false
  end

  (* command line parsing *)
  let parse_size width' height' s =
    try
      let _, w, h, _ = List.find (fun (s', _, _, _) -> s = s') Stdsizes.sizes in
      width' := w;
      height' := h
    with
    | _ ->
      (try
         let index = String.index s 'x' in
         let width = String.sub s 0 index in
         let height = String.sub s (index + 1) (String.length s - index - 1) in
         width' := int_of_string width;
         height' := int_of_string height
       with
      | _ -> failwith ("Unable to parse size: " ^ s))
  ;;

  let parse_format s =
    try
      let s = String.uppercase_ascii s in
      Cfg.auto422 := s = "422";
      Cfg.format := List.assoc s YuvFormat.formats
    with
    | _ -> failwith "Invalid pixel format"
  ;;

  let parse_int i s = i := int_of_string s
  let parse_switch v () = v := not !v

  let args =
    [ "-s", Arg.String (parse_size Cfg.width Cfg.height), " input size"
    ; "-S", Arg.String (parse_size Cfg.widtho Cfg.heighto), " output size"
    ; "-f", Arg.String parse_format, " YUV format"
    ; "-r", Arg.String (parse_int Cfg.framerate), " frame rate (FPS)"
    ; "-d", Arg.Unit (parse_switch Cfg.fullscreen), " start in fullscreen mode"
    ; "-v", Arg.Unit (parse_switch Cfg.verbose), " be verbose"
    ]
  ;;

  let parse args =
    let file_names s =
      if !Cfg.in_file = ""
      then Cfg.in_file := s
      else if !Cfg.diff_file = ""
      then Cfg.diff_file := s
      else failwith "Too many files specified on command line"
    in
    let usage = Sys.argv.(0) ^ " [options] [in_file] [diff_file]\n" in
    Arg.parse (Arg.align args) file_names usage;
    if !Cfg.width = 0 || !Cfg.height = 0 then failwith "No image size was specified";
    if !Cfg.widtho = 0 || !Cfg.heighto = 0
    then (
      Cfg.widtho := !Cfg.width;
      Cfg.heighto := !Cfg.height);
    if !Cfg.diff_file <> "" then Cfg.diff_mode := ExactDiff
  ;;

  let print_opts () =
    if !Cfg.verbose = true
    then (
      fprintf stderr "in-file: %s\n" !Cfg.in_file;
      fprintf stderr "diff-file: %s\n" !Cfg.diff_file;
      fprintf stderr "size-in: %dx%d\n" !Cfg.width !Cfg.height;
      fprintf stderr "size-out: %dx%d\n" !Cfg.widtho !Cfg.heighto;
      fprintf stderr "format: %s\n" (YuvFormat.string_of_format !Cfg.format);
      fprintf stderr "framerate: %d\n" !Cfg.framerate;
      fprintf stderr "fullscreen: %b\n" !Cfg.fullscreen;
      flush stderr)
  ;;

  (* parse commandline *)
  let _ =
    parse args;
    print_opts ()
  ;;
end

open Arg

type ba = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
type ba2 = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array2.t

module FileIO = struct
  open Unix.LargeFile

  type file =
    | UnixFile of Unix.file_descr * ba2 * int
    | CamlFile of in_channel * ba

  let frame_size w h fmt = if YuvFormat.is_planar fmt then w * h * 3 / 2 else w * h * 2

  let open_unix name =
    (* get length of file, and thus number of frames *)
    let length = (Unix.LargeFile.stat name).st_size in
    let size = frame_size !Cfg.width !Cfg.height !Cfg.format in
    let num_frames = Int64.to_int (Int64.div length (Int64.of_int size)) in
    (* open file *)
    let f = open_in_bin name in
    let f = Unix.descr_of_in_channel f in
    (* map file *)
    let m =
      Unix.map_file
        f
        ~pos:0L
        Bigarray.int8_unsigned
        Bigarray.c_layout
        false
        [| num_frames; size |]
      |> Bigarray.array2_of_genarray
    in
    UnixFile (f, m, max 1 num_frames)
  ;;

  let open_stdin () =
    let size = frame_size !Cfg.width !Cfg.height !Cfg.format in
    let ba = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout size in
    CamlFile (stdin, ba)
  ;;

  (* open file, or redirect from stdin *)
  let open_file_or_stdin name =
    try if name = "" then open_stdin () else open_unix name with
    | _ -> failwith ("Failed to open file " ^ name)
  ;;

  let open_file_or_none name =
    try if name = "" then None else Some (open_unix name) with
    | _ -> failwith ("Failed to open file " ^ name)
  ;;

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
          b.{n} <- input_byte f;
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

  let off_pixel_420_planar flip =
    let w = !Cfg.width in
    let h = !Cfg.height in
    let ou = w * h in
    let ov = ou + (w * h / 4) in
    let ou, ov = if flip then ov, ou else ou, ov in
    ( (fun x y -> x + (y * w))
    , (fun x y -> ou + (x / 2) + (y / 2 * (w / 2)))
    , fun x y -> ov + (x / 2) + (y / 2 * (w / 2)) )
  ;;

  let off_pixel_422_packed oy ou ov =
    let w = !Cfg.width in
    ( (fun x y -> (x * 2) + (y * w * 2) + oy)
    , (fun x y -> (x / 2 * 4) + (y * w * 2) + ou)
    , fun x y -> (x / 2 * 4) + (y * w * 2) + ov )
  ;;

  let oy, ou, ov =
    if !Cfg.format = Sdl.Pixel.format_yuy2
    then off_pixel_422_packed 0 1 3
    else if !Cfg.format = Sdl.Pixel.format_uyvy
    then off_pixel_422_packed 1 0 2
    else if !Cfg.format = Sdl.Pixel.format_yvyu
    then off_pixel_422_packed 0 3 1
    else if !Cfg.format = Sdl.Pixel.format_yv12
    then off_pixel_420_planar true
    else if !Cfg.format = Sdl.Pixel.format_iyuv
    then off_pixel_420_planar false
    else failwith "invalid yuv format"
  ;;

  let copy422_to_yuy2 =
    let b = ref None in
    fun f ->
      let t = get_buffer f b in
      let width, height = !Cfg.width, !Cfg.height in
      for y = 0 to height - 1 do
        let h = width * y in
        for x = 0 to width - 1 do
          t.{(h * 2) + (x * 2)} <- f.{h + x}
        done
      done;
      let c = width * height in
      for y = 0 to height - 1 do
        let h = width / 2 * y in
        for x = 0 to (width / 2) - 1 do
          t.{(h * 4) + (x * 4) + 1} <- f.{c + h + x}
        done
      done;
      let c = c + (width * height / 2) in
      for y = 0 to height - 1 do
        let h = width / 2 * y in
        for x = 0 to (width / 2) - 1 do
          t.{(h * 4) + (x * 4) + 3} <- f.{c + h + x}
        done
      done;
      t
  ;;

  let draw_plane =
    let b = ref None in
    fun f ->
      let t = get_buffer f b in
      let loop f =
        loop 0 (!Cfg.width - 1) 1 (fun x ->
            loop 0 (!Cfg.height - 1) 1 (fun y ->
                let oy, ou, ov = oy x y, ou x y, ov x y in
                let vy, vu, vv = f oy ou ov in
                t.{oy} <- vy;
                t.{ou} <- vu;
                t.{ov} <- vv))
      in
      (match !Cfg.show_plane with
      | All -> ()
      | Y -> loop (fun oy _ou _ov -> f.{oy}, 128, 128)
      | U -> loop (fun _oy ou _ov -> 256, f.{ou}, 128)
      | V -> loop (fun _oy _ou ov -> 256, 128, f.{ov}));
      t
  ;;

  let draw_diff =
    let b = ref None in
    fun f1 (f2 : ba) ->
      let t = get_buffer f1 b in
      loop 0 (!Cfg.width - 1) 1 (fun x ->
          loop 0 (!Cfg.height - 1) 1 (fun y ->
              let oy, ou, ov = oy x y, ou x y, ov x y in
              t.{oy} <- abs (f1.{oy} - f2.{oy});
              t.{ou} <- ((f1.{oy} - f2.{oy}) / 2) + 128;
              t.{ov} <- ((f1.{oy} - f2.{oy}) / 2) + 128));
      t
  ;;

  let draw_exact_diff =
    let b = ref None in
    fun f1 (f2 : ba) ->
      let size = Bigarray.Array1.dim f1 in
      let t = get_buffer f1 b in
      for i = 0 to size - 1 do
        t.{i} <- (if f1.{i} <> f2.{i} then 255 else 0)
      done;
      loop 0 (!Cfg.width - 1) 1 (fun x ->
          loop 0 (!Cfg.height - 1) 1 (fun y ->
              let oy, ou, ov = oy x y, ou x y, ov x y in
              t.{oy}
                <- (if f1.{oy} <> f2.{oy} || f1.{ou} <> f2.{ou} || f1.{ov} <> f2.{ov}
                   then 255
                   else 0);
              t.{ou} <- 128;
              t.{ov} <- 128));
      t
  ;;

  let draw_grid =
    let b = ref None in
    fun f ->
      let t = get_buffer f b in
      Bigarray.Array1.blit f t;
      loop 16 (!Cfg.width - 1) 16 (fun x ->
          loop 0 (!Cfg.height - 1) 1 (fun y ->
              t.{oy x y} <- 255;
              t.{ou x y} <- 128;
              t.{ov x y} <- 128));
      loop 16 (!Cfg.height - 1) 16 (fun y ->
          loop 0 (!Cfg.width - 1) 1 (fun x ->
              t.{oy x y} <- 255;
              t.{ou x y} <- 128;
              t.{ov x y} <- 128));
      t
  ;;

  let run f1 f2 =
    (* convert 422 to yuy2 *)
    let f1, f2 =
      match !Cfg.auto422, f2 with
      | true, Some f2 -> copy422_to_yuy2 f1, Some (copy422_to_yuy2 f2)
      | true, None -> copy422_to_yuy2 f1, None
      | _ -> f1, f2
    in
    (* select plane *)
    let f1, f2 =
      match !Cfg.show_plane, f2 with
      | All, _ -> f1, f2
      | _, Some f2 -> draw_plane f1, Some (draw_plane f2)
      | _, None -> draw_plane f1, None
    in
    (* difference *)
    let f =
      match f2 with
      | None -> f1
      | Some f2 ->
        (match !Cfg.diff_mode with
        | NoDiff -> f1
        | OtherFile -> f2
        | Diff -> draw_diff f1 f2
        | ExactDiff -> draw_exact_diff f1 f2)
    in
    (* grid *)
    let f = if !Cfg.grid then draw_grid f else f in
    f
  ;;
end

(* app main loop *)
let main () =
  (* simple resource management - undo what was done *)
  let cleanup = ref [] in
  let do_cleanup () = List.iter (fun f -> f ()) !cleanup in
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
      Sdl.Window.(if !Cfg.fullscreen then fullscreen_desktop else windowed)
    in
    let window =
      create_resource
        (Sdl.create_window
           ~w:!Cfg.widtho
           ~h:!Cfg.heighto
           "OCaml YUV Player"
           (window_flags ()))
        Sdl.destroy_window
    in
    let renderer = create_resource (Sdl.create_renderer window) Sdl.destroy_renderer in
    let _ = Sdl.render_set_logical_size renderer !Cfg.width !Cfg.height in
    let _ = Sdl.set_hint Sdl.Hint.render_scale_quality "linear" in
    let _ = Sdl.render_clear renderer in
    let display =
      create_resource
        (Sdl.create_texture
           renderer
           !Cfg.format
           Sdl.Texture.(access_streaming)
           ~w:!Cfg.width
           ~h:!Cfg.height)
        Sdl.destroy_texture
    in
    (* open file(s) *)
    let fin = FileIO.open_file_or_stdin !Cfg.in_file in
    let fref = FileIO.open_file_or_none !Cfg.diff_file in
    let total_frames =
      match fin with
      | FileIO.UnixFile (_, _, n) -> n
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
      let open Sdl.K in
      if key = escape
      then finish := true
      else if key = tab
      then (
        Cfg.fullscreen := not !Cfg.fullscreen;
        ignore (Sdl.set_window_fullscreen window (window_flags ()));
        redraw := true;
        ignore (Sdl.render_clear renderer))
      else if key = return || key = space
      then play := not !play
      else if key = right
      then (
        play := false;
        set_frame_no (get_frame_no () + 1);
        redraw := true)
      else if key = left
      then (
        play := false;
        set_frame_no (get_frame_no () - 1);
        redraw := true)
      else if key = home || key = k0
      then (
        play := false;
        set_frame_no 0;
        redraw := true)
      else if key = kend
      then (
        play := false;
        set_frame_no (total_frames - 1);
        redraw := true)
      else if key = g
      then (
        Cfg.grid := not !Cfg.grid;
        redraw := true)
      else if key = d
      then (
        Cfg.diff_mode := next_diff_mode !Cfg.diff_mode;
        redraw := true)
      else if key = y
      then (
        Cfg.show_plane := next_plane_mode !Cfg.show_plane;
        redraw := true)
      else (
        let num =
          if key = k1
          then 1
          else if key = k2
          then 2
          else if key = k3
          then 3
          else if key = k4
          then 4
          else if key = k5
          then 5
          else if key = k6
          then 6
          else if key = k7
          then 7
          else if key = k8
          then 8
          else if key = k9
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
    let is_planar = YuvFormat.is_planar !Cfg.format in
    let display_frame frame_no =
      if !redraw
      then (
        let b1 = FileIO.read_file fin frame_no in
        let b2 = FileIO.read_file_opt fref frame_no in
        let b = Transform.run b1 b2 in
        ignore
          (Sdl.update_texture
             display
             None
             b
             (if is_planar then !Cfg.width else !Cfg.width * 2));
        ignore (Sdl.render_copy renderer display);
        ignore (Sdl.render_present renderer);
        redraw := false;
        Sdl.set_window_title window (sprintf "[%i] oplay" frame_no))
    in
    (* playout at given rate *)
    let ftimems () = Int32.to_float (Sdl.get_ticks ()) in
    let lasttime = ref (ftimems ()) in
    let playout =
      let lapse = 1000.0 /. float_of_int !Cfg.framerate in
      fun () ->
        if !play = true
        then (
          let newtime = ftimems () in
          if newtime -. !lasttime >= lapse
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
      if !finish = false
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

(* run main *)
let _ = main ()
