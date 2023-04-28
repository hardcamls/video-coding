open! Core
open Hardcaml
open Hardcaml_jpeg
open Hardcaml_waveterm
module Scan_controller = Scan_controller.New
module Sim = Cyclesim.With_interface (Scan_controller.I) (Scan_controller.O)

let display_rules =
  let port ?(wave_format = Wave_format.Bit_or Hex) name =
    Display_rule.port_name_is name ~wave_format
  in
  Display_rule.
    [ port "clock"
    ; port "clear"
    ; port "start"
    ; port "STATE" ~wave_format:(Index Scan_controller.State.strings)
    ; port "starter"
    ; port "all_done"
    ; port "blk_x" ~wave_format:Unsigned_int
    ; port "blk_y" ~wave_format:Unsigned_int
    ; port "x_pos" ~wave_format:Unsigned_int
    ; port "y_pos" ~wave_format:Unsigned_int
    ; port "scan_index" ~wave_format:Unsigned_int
    ; port "max_horz_sampling" ~wave_format:Unsigned_int
    ; port "max_vert_sampling" ~wave_format:Unsigned_int
    ; port "width_in_blocks" ~wave_format:Unsigned_int
    ; port "height_in_blocks" ~wave_format:Unsigned_int
    ; port "component_width" ~wave_format:Unsigned_int
    ; port "component_height" ~wave_format:Unsigned_int
    ; port_name_matches
        (Re.compile (Re.Posix.re "^scan\\$"))
        ~wave_format:(Bit_or Unsigned_int)
    ; port_name_matches (Re.compile (Re.Posix.re "^sos\\$")) ~wave_format:(Bit_or Hex)
    ; port_name_matches (Re.compile (Re.Posix.re "^sof\\$")) ~wave_format:(Bit_or Hex)
    ]
;;

let ( <--. ) a b = a := Bits.of_int ~width:(Bits.width !a) b

type component =
  { ident : int
  ; vert : int
  ; horz : int
  ; qtab : int
  }

type scan =
  { ident : int
  ; dc : int
  ; ac : int
  }

let write_component
    ~(inputs : _ Scan_controller.I.t)
    ~cycle
    ~address
    { ident; vert; horz; qtab }
  =
  inputs.sof.component.identifier <--. ident;
  inputs.sof.component.fields.vertical_sampling_factor <--. vert;
  inputs.sof.component.fields.horizontal_sampling_factor <--. horz;
  inputs.sof.component.fields.quantization_table_identifier <--. qtab;
  inputs.sof.component_address <--. address;
  inputs.sof.component_write := Bits.vdd;
  cycle ();
  inputs.sof.component_write := Bits.gnd
;;

let write_scan ~(inputs : _ Scan_controller.I.t) ~cycle { ident; dc; ac } =
  inputs.sos.scan_selector.identifier <--. ident;
  inputs.sos.scan_selector.fields.dc_coef_selector <--. dc;
  inputs.sos.scan_selector.fields.ac_coef_selector <--. ac;
  inputs.sos.write_scan_selector := Bits.vdd;
  cycle ();
  inputs.sos.write_scan_selector := Bits.gnd
;;

let test ?(waves = false) ~width ~height components scans =
  let sim =
    Sim.create
      ~config:Cyclesim.Config.trace_all
      (Scan_controller.create (Scope.create ()))
  in
  let waves, sim =
    if waves
    then (
      let waves, sim = Waveform.create sim in
      Some waves, sim)
    else None, sim
  in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  inputs.clocking.clear := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clocking.clear := Bits.gnd;
  inputs.all_done := Bits.vdd;
  inputs.sof.frame_header.width <--. width;
  inputs.sof.frame_header.height <--. height;
  for address = 0 to Array.length components - 1 do
    write_component
      ~inputs
      ~cycle:(fun () -> Cyclesim.cycle sim)
      ~address
      components.(address)
  done;
  inputs.sos.header.number_of_image_components <--. Array.length scans;
  for i = 0 to Array.length scans - 1 do
    write_scan ~inputs ~cycle:(fun () -> Cyclesim.cycle sim) scans.(i)
  done;
  Cyclesim.cycle sim;
  inputs.start := Bits.vdd;
  let cycle () =
    if Bits.to_bool !(outputs.starter)
    then (
      let x_pos = Bits.to_int !(outputs.x_pos) in
      let y_pos = Bits.to_int !(outputs.y_pos) in
      let scan_index = Bits.to_int !(outputs.scan_index) in
      print_s [%message (scan_index : int) (x_pos : int) (y_pos : int)]);
    Cyclesim.cycle sim
  in
  cycle ();
  inputs.start := Bits.gnd;
  cycle ();
  while not (Bits.to_bool !(outputs.done_)) do
    cycle ()
  done;
  cycle ();
  cycle ();
  waves
;;

let%expect_test "16x8, single scan" =
  let components = [| { ident = 7; vert = 1; horz = 1; qtab = 0 } |] in
  let scans = [| { ident = 7; dc = 0; ac = 0 } |] in
  let waves = test ~waves:false ~width:16 ~height:8 components scans in
  Option.iter
    waves
    ~f:(Waveform.print ~display_width:160 ~display_height:70 ~wave_width:1 ~display_rules);
  [%expect
    {|
    ((scan_index 0) (x_pos 0) (y_pos 0))
    ((scan_index 0) (x_pos 8) (y_pos 0)) |}]
;;

let%expect_test "16x16, single scan, vertical order" =
  let components = [| { ident = 7; vert = 2; horz = 1; qtab = 0 } |] in
  let scans = [| { ident = 7; dc = 0; ac = 0 } |] in
  let waves = test ~waves:false ~width:16 ~height:16 components scans in
  Option.iter
    waves
    ~f:(Waveform.print ~display_width:160 ~display_height:70 ~wave_width:1 ~display_rules);
  [%expect
    {|
    ((scan_index 0) (x_pos 0) (y_pos 0))
    ((scan_index 0) (x_pos 0) (y_pos 8))
    ((scan_index 0) (x_pos 8) (y_pos 0))
    ((scan_index 0) (x_pos 8) (y_pos 8)) |}]
;;

let%expect_test "16x16, inorder scans" =
  let components =
    [| { ident = 1; vert = 2; horz = 2; qtab = 0 }
     ; { ident = 2; vert = 2; horz = 1; qtab = 1 }
     ; { ident = 3; vert = 1; horz = 1; qtab = 1 }
    |]
  in
  let scans =
    [| { ident = 1; dc = 0; ac = 0 }
     ; { ident = 2; dc = 1; ac = 1 }
     ; { ident = 3; dc = 1; ac = 1 }
    |]
  in
  let waves = test ~waves:false ~width:16 ~height:16 components scans in
  Option.iter
    waves
    ~f:(Waveform.print ~display_width:160 ~display_height:70 ~wave_width:1 ~display_rules);
  [%expect
    {|
    ((scan_index 0) (x_pos 0) (y_pos 0))
    ((scan_index 0) (x_pos 8) (y_pos 0))
    ((scan_index 0) (x_pos 0) (y_pos 8))
    ((scan_index 0) (x_pos 8) (y_pos 8))
    ((scan_index 1) (x_pos 0) (y_pos 0))
    ((scan_index 1) (x_pos 0) (y_pos 8))
    ((scan_index 2) (x_pos 0) (y_pos 0)) |}]
;;

let components =
  [| { ident = 7; vert = 2; horz = 2; qtab = 0 }
   ; { ident = 9; vert = 2; horz = 1; qtab = 1 }
   ; { ident = 2; vert = 1; horz = 1; qtab = 1 }
  |]
;;

let scans =
  [| { ident = 9; dc = 0; ac = 0 }
   ; { ident = 2; dc = 1; ac = 1 }
   ; { ident = 7; dc = 1; ac = 1 }
  |]
;;

let%expect_test "16x16, out of order scans" =
  let waves = test ~waves:false ~width:16 ~height:16 components scans in
  Option.iter
    waves
    ~f:(Waveform.print ~display_width:160 ~display_height:70 ~wave_width:1 ~display_rules);
  [%expect
    {|
    ((scan_index 0) (x_pos 0) (y_pos 0))
    ((scan_index 0) (x_pos 0) (y_pos 8))
    ((scan_index 1) (x_pos 0) (y_pos 0))
    ((scan_index 2) (x_pos 0) (y_pos 0))
    ((scan_index 2) (x_pos 8) (y_pos 0))
    ((scan_index 2) (x_pos 0) (y_pos 8))
    ((scan_index 2) (x_pos 8) (y_pos 8)) |}]
;;

let%expect_test "16x32" =
  let waves = test ~waves:false ~width:16 ~height:32 components scans in
  Option.iter
    waves
    ~f:(Waveform.print ~display_width:160 ~display_height:70 ~wave_width:1 ~display_rules);
  [%expect
    {|
    ((scan_index 0) (x_pos 0) (y_pos 0))
    ((scan_index 0) (x_pos 0) (y_pos 8))
    ((scan_index 1) (x_pos 0) (y_pos 0))
    ((scan_index 2) (x_pos 0) (y_pos 0))
    ((scan_index 2) (x_pos 8) (y_pos 0))
    ((scan_index 2) (x_pos 0) (y_pos 8))
    ((scan_index 2) (x_pos 8) (y_pos 8))
    ((scan_index 0) (x_pos 0) (y_pos 16))
    ((scan_index 0) (x_pos 0) (y_pos 24))
    ((scan_index 1) (x_pos 0) (y_pos 8))
    ((scan_index 2) (x_pos 0) (y_pos 16))
    ((scan_index 2) (x_pos 8) (y_pos 16))
    ((scan_index 2) (x_pos 0) (y_pos 24))
    ((scan_index 2) (x_pos 8) (y_pos 24)) |}]
;;

let%expect_test "32x16" =
  let waves = test ~waves:false ~width:32 ~height:16 components scans in
  Option.iter
    waves
    ~f:(Waveform.print ~display_width:160 ~display_height:70 ~wave_width:1 ~display_rules);
  [%expect
    {|
    ((scan_index 0) (x_pos 0) (y_pos 0))
    ((scan_index 0) (x_pos 0) (y_pos 8))
    ((scan_index 1) (x_pos 0) (y_pos 0))
    ((scan_index 2) (x_pos 0) (y_pos 0))
    ((scan_index 2) (x_pos 8) (y_pos 0))
    ((scan_index 2) (x_pos 0) (y_pos 8))
    ((scan_index 2) (x_pos 8) (y_pos 8))
    ((scan_index 0) (x_pos 8) (y_pos 0))
    ((scan_index 0) (x_pos 8) (y_pos 8))
    ((scan_index 1) (x_pos 8) (y_pos 0))
    ((scan_index 2) (x_pos 16) (y_pos 0))
    ((scan_index 2) (x_pos 24) (y_pos 0))
    ((scan_index 2) (x_pos 16) (y_pos 8))
    ((scan_index 2) (x_pos 24) (y_pos 8)) |}]
;;

let%expect_test "420 48x48" =
  let components =
    [| { ident = 7; vert = 2; horz = 2; qtab = 0 }
     ; { ident = 9; vert = 1; horz = 1; qtab = 1 }
     ; { ident = 2; vert = 1; horz = 1; qtab = 1 }
    |]
  in
  let scans =
    [| { ident = 7; dc = 0; ac = 0 }
     ; { ident = 9; dc = 1; ac = 1 }
     ; { ident = 2; dc = 1; ac = 1 }
    |]
  in
  let waves = test ~waves:false ~width:48 ~height:48 components scans in
  Option.iter
    waves
    ~f:(Waveform.print ~display_width:160 ~display_height:70 ~wave_width:1 ~display_rules);
  [%expect
    {|
    ((scan_index 0) (x_pos 0) (y_pos 0))
    ((scan_index 0) (x_pos 8) (y_pos 0))
    ((scan_index 0) (x_pos 0) (y_pos 8))
    ((scan_index 0) (x_pos 8) (y_pos 8))
    ((scan_index 1) (x_pos 0) (y_pos 0))
    ((scan_index 2) (x_pos 0) (y_pos 0))
    ((scan_index 0) (x_pos 16) (y_pos 0))
    ((scan_index 0) (x_pos 24) (y_pos 0))
    ((scan_index 0) (x_pos 16) (y_pos 8))
    ((scan_index 0) (x_pos 24) (y_pos 8))
    ((scan_index 1) (x_pos 8) (y_pos 0))
    ((scan_index 2) (x_pos 8) (y_pos 0))
    ((scan_index 0) (x_pos 32) (y_pos 0))
    ((scan_index 0) (x_pos 40) (y_pos 0))
    ((scan_index 0) (x_pos 32) (y_pos 8))
    ((scan_index 0) (x_pos 40) (y_pos 8))
    ((scan_index 1) (x_pos 16) (y_pos 0))
    ((scan_index 2) (x_pos 16) (y_pos 0))
    ((scan_index 0) (x_pos 0) (y_pos 16))
    ((scan_index 0) (x_pos 8) (y_pos 16))
    ((scan_index 0) (x_pos 0) (y_pos 24))
    ((scan_index 0) (x_pos 8) (y_pos 24))
    ((scan_index 1) (x_pos 0) (y_pos 8))
    ((scan_index 2) (x_pos 0) (y_pos 8))
    ((scan_index 0) (x_pos 16) (y_pos 16))
    ((scan_index 0) (x_pos 24) (y_pos 16))
    ((scan_index 0) (x_pos 16) (y_pos 24))
    ((scan_index 0) (x_pos 24) (y_pos 24))
    ((scan_index 1) (x_pos 8) (y_pos 8))
    ((scan_index 2) (x_pos 8) (y_pos 8))
    ((scan_index 0) (x_pos 32) (y_pos 16))
    ((scan_index 0) (x_pos 40) (y_pos 16))
    ((scan_index 0) (x_pos 32) (y_pos 24))
    ((scan_index 0) (x_pos 40) (y_pos 24))
    ((scan_index 1) (x_pos 16) (y_pos 8))
    ((scan_index 2) (x_pos 16) (y_pos 8))
    ((scan_index 0) (x_pos 0) (y_pos 32))
    ((scan_index 0) (x_pos 8) (y_pos 32))
    ((scan_index 0) (x_pos 0) (y_pos 40))
    ((scan_index 0) (x_pos 8) (y_pos 40))
    ((scan_index 1) (x_pos 0) (y_pos 16))
    ((scan_index 2) (x_pos 0) (y_pos 16))
    ((scan_index 0) (x_pos 16) (y_pos 32))
    ((scan_index 0) (x_pos 24) (y_pos 32))
    ((scan_index 0) (x_pos 16) (y_pos 40))
    ((scan_index 0) (x_pos 24) (y_pos 40))
    ((scan_index 1) (x_pos 8) (y_pos 16))
    ((scan_index 2) (x_pos 8) (y_pos 16))
    ((scan_index 0) (x_pos 32) (y_pos 32))
    ((scan_index 0) (x_pos 40) (y_pos 32))
    ((scan_index 0) (x_pos 32) (y_pos 40))
    ((scan_index 0) (x_pos 40) (y_pos 40))
    ((scan_index 1) (x_pos 16) (y_pos 16))
    ((scan_index 2) (x_pos 16) (y_pos 16)) |}]
;;
