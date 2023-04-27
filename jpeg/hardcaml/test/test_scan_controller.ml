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
    ; port "STATE"
    ; port "scan_address" ~wave_format:Unsigned_int
    ; port "x_pos" ~wave_format:Unsigned_int
    ; port "y_pos" ~wave_format:Unsigned_int
    ; port_name_matches (Re.compile (Re.Posix.re "^scan\\$")) ~wave_format:(Bit_or Hex)
    ; port_name_matches (Re.compile (Re.Posix.re "^sof\\$")) ~wave_format:(Bit_or Hex)
    ; port_name_matches (Re.compile (Re.Posix.re "^sos\\$")) ~wave_format:(Bit_or Hex)
    ]
;;

let ( <--. ) a b = a := Bits.of_int ~width:(Bits.width !a) b

let%expect_test "" =
  let sim =
    Sim.create
      ~config:Cyclesim.Config.trace_all
      (Scan_controller.create (Scope.create ()))
  in
  let waves, sim = Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  inputs.clocking.clear := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clocking.clear := Bits.gnd;
  inputs.sof.frame_header.width <--. 16;
  inputs.sof.frame_header.height <--. 16;
  let write_component ~address ~ident ~vert ~horz ~qtab =
    inputs.sof.component.identifier <--. ident;
    inputs.sof.component.fields.vertical_sampling_factor <--. vert;
    inputs.sof.component.fields.horizontal_sampling_factor <--. horz;
    inputs.sof.component.fields.quantization_table_identifier <--. qtab;
    inputs.sof.component_address <--. address;
    inputs.sof.component_write := Bits.vdd;
    Cyclesim.cycle sim;
    inputs.sof.component_write := Bits.gnd
  in
  write_component ~address:0 ~ident:7 ~vert:2 ~horz:2 ~qtab:0;
  write_component ~address:1 ~ident:9 ~vert:2 ~horz:1 ~qtab:1;
  write_component ~address:2 ~ident:2 ~vert:1 ~horz:1 ~qtab:1;
  inputs.sos.header.number_of_image_components <--. 3;
  let write_scan ~ident ~dc ~ac =
    inputs.sos.scan_selector.identifier <--. ident;
    inputs.sos.scan_selector.fields.dc_coef_selector <--. dc;
    inputs.sos.scan_selector.fields.ac_coef_selector <--. ac;
    inputs.sos.write_scan_selector := Bits.vdd;
    Cyclesim.cycle sim;
    inputs.sos.write_scan_selector := Bits.gnd
  in
  write_scan ~ident:9 ~dc:0 ~ac:0;
  write_scan ~ident:2 ~dc:1 ~ac:1;
  write_scan ~ident:7 ~dc:1 ~ac:1;
  Cyclesim.cycle sim;
  inputs.start := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.start := Bits.gnd;
  for _ = 0 to 10 do
    Cyclesim.cycle sim
  done;
  Waveform.print ~display_width:100 ~display_height:70 ~wave_width:1 ~display_rules waves;
  [%expect{|
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────┐
    │clock             ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─│
    │                  ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ │
    │clear             ││────┐                                                                         │
    │                  ││    └─────────────────────────────────────────────────────────────────────────│
    │start             ││                                ┌───┐                                         │
    │                  ││────────────────────────────────┘   └─────────────────────────────────────────│
    │                  ││────────────────────────────────────┬───┬───────┬───┬───┬───┬───────────────┬─│
    │STATE             ││ 0                                  │1  │2      │1  │2  │1  │2              │1│
    │                  ││────────────────────────────────────┴───┴───────┴───┴───┴───┴───────────────┴─│
    │                  ││────────────────────┬───┬───┬───────┬───────────┬───────┬───────────────────┬─│
    │scan_address      ││ 0                  │1  │2  │3      │0          │1      │2                  │3│
    │                  ││────────────────────┴───┴───┴───────┴───────────┴───────┴───────────────────┴─│
    │                  ││────────────────────────────────────────────────┬───┬───┬───┬───┬───────┬───┬─│
    │x_pos             ││ 0                                              │8  │0  │8  │0  │8      │16 │2│
    │                  ││────────────────────────────────────────────────┴───┴───┴───┴───┴───────┴───┴─│
    │                  ││────────────────────────────────────────────┬───┬───────────────────┬───────┬─│
    │y_pos             ││ 0                                          │8  │0                  │8      │0│
    │                  ││────────────────────────────────────────────┴───┴───────────────────┴───────┴─│
    │scan$ac           ││                                                    ┌─────────────────────────│
    │                  ││────────────────────────────────────────────────────┘                         │
    │                  ││────────────────────────────────────────┬───────────┬───────┬─────────────────│
    │scan$component_ind││ 0                                      │1          │2      │0                │
    │                  ││────────────────────────────────────────┴───────────┴───────┴─────────────────│
    │scan$dc           ││                                                    ┌─────────────────────────│
    │                  ││────────────────────────────────────────────────────┘                         │
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │scan$dc_pred      ││ 0000                                                                         │
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │                  ││────────────────────────────────────────────────┬───┬───┬───┬───┬───────┬───┬─│
    │scan$x_pos        ││ 0000                                           │00.│00.│00.│00.│0008   │00.│0│
    │                  ││────────────────────────────────────────────────┴───┴───┴───┴───┴───────┴───┴─│
    │                  ││────────────────────────────────────────────┬───┬───────────────────┬───────┬─│
    │scan$y_pos        ││ 0000                                       │00.│0000               │0008   │0│
    │                  ││────────────────────────────────────────────┴───┴───────────────────┴───────┴─│
    │                  ││────────┬───┬─────────────────────────────────────────────────────────────────│
    │sof$component_addr││ 0      │1  │2                                                                │
    │                  ││────────┴───┴─────────────────────────────────────────────────────────────────│
    │sof$component_writ││    ┌───────────┐                                                             │
    │                  ││────┘           └─────────────────────────────────────────────────────────────│
    │                  ││────┬───┬─────────────────────────────────────────────────────────────────────│
    │sof$horizontal_sam││ 0  │2  │1                                                                    │
    │                  ││────┴───┴─────────────────────────────────────────────────────────────────────│
    │                  ││────┬───┬───┬─────────────────────────────────────────────────────────────────│
    │sof$identifier    ││ 00 │07 │09 │02                                                               │
    │                  ││────┴───┴───┴─────────────────────────────────────────────────────────────────│
    │                  ││────────┬─────────────────────────────────────────────────────────────────────│
    │sof$quantization_t││ 00     │01                                                                   │
    │                  ││────────┴─────────────────────────────────────────────────────────────────────│
    │                  ││────┬───────┬─────────────────────────────────────────────────────────────────│
    │sof$vertical_sampl││ 0  │2      │1                                                                │
    │                  ││────┴───────┴─────────────────────────────────────────────────────────────────│
    │                  ││────────────────────┬─────────────────────────────────────────────────────────│
    │sos$ac_coef_select││ 0                  │1                                                        │
    │                  ││────────────────────┴─────────────────────────────────────────────────────────│
    │                  ││────────────────────┬─────────────────────────────────────────────────────────│
    │sos$dc_coef_select││ 0                  │1                                                        │
    │                  ││────────────────────┴─────────────────────────────────────────────────────────│
    │                  ││────────────────┬───┬───┬─────────────────────────────────────────────────────│
    │sos$identifier    ││ 00             │09 │02 │07                                                   │
    │                  ││────────────────┴───┴───┴─────────────────────────────────────────────────────│
    │sos$write_scan_sel││                ┌───────────┐                                                 │
    │                  ││────────────────┘           └─────────────────────────────────────────────────│
    │                  ││                                                                              │
    │                  ││                                                                              │
    │                  ││                                                                              │
    │                  ││                                                                              │
    │                  ││                                                                              │
    │                  ││                                                                              │
    └──────────────────┘└──────────────────────────────────────────────────────────────────────────────┘ |}]
;;
