open Base
open Hardcaml
open Signal
module Var = Always.Variable

module Ctrl = struct
  type 'a t =
    { codeblock_decoder : 'a
    ; idct : 'a
    ; output : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

let max_components = 4
let log_max_components = Int.ceil_log2 max_components
let max_scans = 4
let log_max_scans = Int.ceil_log2 max_scans

(* 1 for baseline, 2 for extended+progressive *)

module Core = struct
  module I = struct
    type 'a t =
      { clocking : 'a Clocking.t
      ; start : 'a
      ; sof : 'a Markers.Sof.Fields.t [@rtlprefix "sof$"]
      ; sos : 'a Markers.Sos.Fields.t [@rtlprefix "sos$"]
      ; dc_pred_in : 'a [@bits 12]
      ; dc_pred_write : 'a
      ; all_done : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { done_ : 'a
      ; dc_pred_out : 'a [@bits 12]
      ; ac_table_select : 'a [@bits log_max_scans]
      ; dc_table_select : 'a [@bits log_max_scans]
      ; qnt_table_select : 'a [@bits log_max_components]
      ; x_pos : 'a [@bits 16]
      ; y_pos : 'a [@bits 16]
      ; scan_index : 'a [@bits log_max_scans]
      ; starter : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  (* As components are decoded, map them to an index, and provide a way to reverse the mapping. *)
  let component_mapping clocking (sof : _ Markers.Sof.Fields.t) selector =
    let component_address = sof.component_address.:[log_max_components - 1, 0] in
    let component_address_onehot = binary_to_onehot component_address in
    let component_map =
      Array.init max_components ~f:(fun index ->
          Clocking.reg
            clocking
            ~enable:(sof.component_write &: component_address_onehot.:(index))
            sof.component.identifier)
    in
    let component_identifier =
      priority_select
        (List.init max_components ~f:(fun index ->
             { With_valid.valid = component_map.(index) ==: selector
             ; value = Signal.of_int ~width:log_max_components index
             }))
    in
    component_identifier
  ;;

  module State = struct
    type t =
      | Headers
      | Component_start
      | Component_read
      | Blocks
    [@@deriving sexp_of, compare, enumerate, variants]

    let strings = List.map all ~f:(fun v -> Sexp.to_string (sexp_of_t v))
  end

  module Scan_state = struct
    type 'a t =
      { dc : 'a [@bits log_max_scans]
      ; ac : 'a [@bits log_max_scans]
      ; component_index : 'a [@bits log_max_components]
      ; x_pos : 'a [@bits 13]
      ; y_pos : 'a [@bits 13]
      ; dc_pred : 'a [@bits 12]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let max_sampling_factors (i : _ I.t) clear =
    let clocking = { i.clocking with clear = i.clocking.clear |: clear } in
    let enable = i.sof.component_write in
    let max_reg factor =
      Clocking.reg_fb clocking ~width:(width factor) ~enable ~f:(fun d ->
          mux2 (factor >: d) factor d)
    in
    let max_horz =
      let factor = i.sof.component.fields.horizontal_sampling_factor in
      max_reg factor
    in
    let max_vert =
      let factor = i.sof.component.fields.vertical_sampling_factor in
      max_reg factor
    in
    max_horz, max_vert
  ;;

  let padded_width_and_height_in_blocks (i : _ I.t) ~max_horz_sampling ~max_vert_sampling =
    let width = i.sof.frame_header.width in
    let height = i.sof.frame_header.height in
    let rounding max =
      mux max ([ 0; 7; 15; 23; 31 ] |> List.map ~f:(Signal.of_int ~width:16))
    in
    let width = width +: rounding max_horz_sampling in
    let height = height +: rounding max_vert_sampling in
    let round_to max x =
      mux
        max
        [ drop_bottom x 3
        ; drop_bottom x 3
        ; drop_bottom x 4 @: zero 1
        ; drop_bottom x 4 @: zero 1
        ; drop_bottom x 5 @: zero 2
        ]
    in
    round_to max_horz_sampling width, round_to max_vert_sampling height
  ;;

  let component_width_and_height_in_blocks
      ~width_in_blocks
      ~height_in_blocks
      ~max_horz_sampling
      ~max_vert_sampling
      ~horz_sampling
      ~vert_sampling
    =
    let scale max sampling size =
      let scale = max -: sampling in
      log_shift srl size scale
    in
    ( scale max_horz_sampling horz_sampling width_in_blocks
    , scale max_vert_sampling vert_sampling height_in_blocks )
  ;;

  let create scope (i : _ I.t) =
    let ( -- ) = Scope.naming scope in
    let sm = Always.State_machine.create (module State) (Clocking.to_spec i.clocking) in
    let component_identifier =
      component_mapping i.clocking i.sof i.sos.scan_selector.identifier
    in
    let max_horz_sampling, max_vert_sampling =
      max_sampling_factors i gnd (* XXX need to reset between frames *)
    in
    ignore (max_horz_sampling -- "max_horz_sampling" : Signal.t);
    ignore (max_vert_sampling -- "max_vert_sampling" : Signal.t);
    let width_in_blocks, height_in_blocks =
      padded_width_and_height_in_blocks i ~max_horz_sampling ~max_vert_sampling
    in
    ignore (width_in_blocks -- "width_in_blocks" : Signal.t);
    ignore (height_in_blocks -- "height_in_blocks" : Signal.t);
    let component_address = Var.wire ~default:(zero log_max_components) in
    let scan_address = Clocking.Var.reg i.clocking ~width:log_max_scans in
    ignore (scan_address.value -- "scan_address" : Signal.t);
    let scan_address_write = Clocking.Var.reg i.clocking ~width:log_max_scans in
    let scan_address_next = ue scan_address.value +:. 1 in
    ignore (sm.current -- "STATE" : Signal.t);
    let component =
      memory
        max_components
        ~write_port:
          { write_clock = i.clocking.clock
          ; write_address = i.sof.component_address.:[log_max_components - 1, 0]
          ; write_enable = i.sof.component_write
          ; write_data = Markers.Component.Fields.Of_signal.pack i.sof.component.fields
          }
        ~read_address:component_address.value
      |> Markers.Component.Fields.Of_signal.unpack
    in
    let update_scan = Clocking.Var.reg i.clocking ~width:1 in
    let scan_r = Scan_state.Of_always.reg (Clocking.to_spec i.clocking) in
    let component_width, component_height =
      component_width_and_height_in_blocks
        ~width_in_blocks
        ~height_in_blocks
        ~max_horz_sampling
        ~max_vert_sampling
        ~horz_sampling:component.horizontal_sampling_factor
        ~vert_sampling:component.vertical_sampling_factor
    in
    ignore (component_width -- "component_width" : Signal.t);
    ignore (component_height -- "component_height" : Signal.t);
    Scan_state.Of_always.apply_names ~naming_op:( -- ) ~prefix:"scan$" scan_r;
    let scan =
      memory
        max_scans
        ~write_port:
          { write_clock = i.clocking.clock
          ; write_address =
              mux2 update_scan.value scan_address_write.value scan_address.value
          ; write_enable = i.sos.write_scan_selector |: update_scan.value
          ; write_data =
              Scan_state.(
                Of_signal.(
                  pack
                    (mux2 update_scan.value (Of_always.value scan_r)
                    @@ map2
                         port_widths
                         { dc =
                             i.sos.scan_selector.fields.dc_coef_selector.:[( log_max_scans
                                                                             - 1
                                                                           , 0 )]
                         ; ac =
                             i.sos.scan_selector.fields.ac_coef_selector.:[( log_max_scans
                                                                             - 1
                                                                           , 0 )]
                         ; component_index = component_identifier.value
                         ; x_pos = zero 13
                         ; y_pos = zero 13
                         ; dc_pred = zero 12
                         }
                         ~f:(fun w d -> sel_bottom d w))))
          }
        ~read_address:scan_address.value
      |> Scan_state.Of_signal.unpack
    in
    let blk_x = Clocking.Var.reg i.clocking ~width:4 in
    ignore (blk_x.value -- "blk_x" : Signal.t);
    let blk_x_next = blk_x.value +:. 1 in
    let blk_y = Clocking.Var.reg i.clocking ~width:4 in
    ignore (blk_y.value -- "blk_y" : Signal.t);
    let blk_y_next = blk_y.value +:. 1 in
    let x_pos_next = scan_r.x_pos.value +:. 1 in
    let y_pos_next = scan_r.y_pos.value +:. 1 in
    let x_pos_start = Clocking.Var.reg i.clocking ~width:13 in
    let y_pos_start = Clocking.Var.reg i.clocking ~width:13 in
    let last_component =
      scan_address_next ==: i.sos.header.number_of_image_components.:[log_max_scans, 0]
    in
    let last_col = x_pos_next ==: component_width in
    let last_row = y_pos_next ==: component_height in
    let starter = Clocking.Var.reg i.clocking ~width:1 in
    Always.(
      compile
        [ update_scan <-- gnd
        ; starter <-- gnd
        ; sm.switch
            [ ( Headers
              , [ when_
                    i.sos.write_scan_selector
                    [ scan_address <-- lsbs scan_address_next ]
                ; when_ i.start [ scan_address <--. 0; sm.set_next Component_start ]
                ] )
            ; ( Component_start
              , [ (* The scan component write back happens here.  It is read in 
                     the next state - this is needed for scans with a single 
                     component. *)
                  component_address <-- scan.component_index
                ; blk_x <--. 0
                ; blk_y <--. 0
                ; sm.set_next Component_read
                ] )
            ; ( Component_read
              , [ component_address <-- scan.component_index
                ; Scan_state.Of_always.assign scan_r scan
                ; x_pos_start <-- scan.x_pos
                ; y_pos_start <-- scan.y_pos
                ; starter <-- vdd
                ; sm.set_next Blocks
                ] )
            ; ( Blocks
              , [ component_address <-- scan_r.component_index.value
                ; when_ i.dc_pred_write [ scan_r.dc_pred <-- i.dc_pred_in ]
                ; when_
                    (i.all_done &: ~:(starter.value))
                    [ starter <-- vdd
                    ; blk_x <-- blk_x_next
                    ; scan_r.x_pos <-- x_pos_next
                    ; when_
                        (blk_x_next ==: component.horizontal_sampling_factor)
                        [ blk_x <--. 0
                        ; blk_y <-- blk_y_next
                        ; scan_r.x_pos <-- x_pos_start.value
                        ; scan_r.y_pos <-- y_pos_next
                        ; when_
                            (blk_y_next ==: component.vertical_sampling_factor)
                            [ scan_address <-- lsbs scan_address_next
                            ; when_ last_component [ scan_address <--. 0 ]
                            ; scan_address_write <-- scan_address.value
                            ; update_scan <-- vdd
                            ; starter <-- gnd
                            ; blk_y <--. 0
                            ; scan_r.x_pos <-- x_pos_next
                            ; scan_r.y_pos <-- y_pos_start.value
                            ; when_
                                (x_pos_next ==: component_width)
                                [ scan_r.x_pos <--. 0; scan_r.y_pos <-- y_pos_next ]
                            ; sm.set_next Component_start
                            ; when_
                                (last_component &: last_col &: last_row)
                                [ sm.set_next Headers ]
                            ]
                        ]
                    ]
                ] )
            ]
        ]);
    { O.done_ = sm.is Headers
    ; dc_pred_out = scan_r.dc_pred.value
    ; ac_table_select = scan_r.ac.value
    ; dc_table_select = scan_r.dc.value
    ; qnt_table_select =
        component.quantization_table_identifier.:[log_max_components - 1, 0]
    ; x_pos = scan_r.x_pos.value @: zero 3
    ; y_pos = scan_r.y_pos.value @: zero 3
    ; scan_index = scan_address.value
    ; starter = starter.value
    }
  ;;

  let hierarchical scope =
    let module Hier = Hierarchy.In_scope (I) (O) in
    Hier.hierarchical ~scope ~name:"ctrl" create
  ;;
end

module With_pipeline_coontrol = struct
  module I = struct
    type 'a t =
      { clocking : 'a Clocking.t
      ; start : 'a
      ; sof : 'a Markers.Sof.Fields.t [@rtlprefix "sof$"]
      ; sos : 'a Markers.Sos.Fields.t [@rtlprefix "sos$"]
      ; dc_pred_in : 'a [@bits 12]
      ; dc_pred_write : 'a
      ; dones : 'a Ctrl.t [@rtlsuffix "_done"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { done_ : 'a
      ; dc_pred_out : 'a [@bits 12]
      ; ac_table_select : 'a [@bits log_max_scans]
      ; dc_table_select : 'a [@bits log_max_scans]
      ; qnt_table_select : 'a [@bits log_max_components]
      ; x_pos : 'a [@bits 16]
      ; y_pos : 'a [@bits 16]
      ; scan_index : 'a [@bits log_max_scans]
      ; starts : 'a Ctrl.t [@rtlsuffix "_start"]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module State = struct
    type t =
      | Start
      | Process
      | Flush
    [@@deriving sexp_of, compare, enumerate]
  end

  let create scope (i : _ I.t) =
    let ( -- ) = Scope.naming scope in
    let sm = Always.State_machine.create (module State) (Clocking.to_spec i.clocking) in
    ignore (sm.current -- "STATE" : Signal.t);
    let starter = Clocking.Var.reg i.clocking ~width:1 in
    let start_pipe = Clocking.Var.reg i.clocking ~width:3 in
    let all_done =
      let dones = reduce ~f:( &: ) (Ctrl.to_list i.dones) in
      dones &: ~:(starter.value)
    in
    let ctrl =
      Core.hierarchical
        scope
        { Core.I.clocking = i.clocking
        ; start = i.start
        ; sof = i.sof
        ; sos = i.sos
        ; dc_pred_in = i.dc_pred_in
        ; dc_pred_write = i.dc_pred_write
        ; all_done
        }
    in
    Always.(
      compile
        [ starter <-- gnd
        ; sm.switch
            [ Start, [ when_ i.start [ start_pipe <--. 0; sm.set_next Process ] ]
            ; ( Process
              , [ when_
                    all_done
                    [ when_
                        ctrl.starter
                        [ start_pipe <-- start_pipe.value.:[1, 0] @: vdd
                        ; starter <-- vdd
                        ]
                    ; when_ ctrl.done_ [ starter <-- gnd; sm.set_next Flush ]
                    ]
                ] )
            ; ( Flush
              , [ when_
                    all_done
                    [ start_pipe <-- start_pipe.value.:[1, 0] @: gnd
                    ; starter <-- vdd
                    ; when_
                        (start_pipe.value ==:. 0)
                        [ starter <-- gnd; sm.set_next Start ]
                    ]
                ] )
            ]
        ]);
    { O.done_ = sm.is Start
    ; dc_pred_out = ctrl.dc_pred_out
    ; ac_table_select = ctrl.ac_table_select
    ; dc_table_select = ctrl.dc_table_select
    ; qnt_table_select = ctrl.qnt_table_select
    ; x_pos = ctrl.x_pos
    ; y_pos = ctrl.y_pos
    ; scan_index = ctrl.scan_index
    ; starts =
        { codeblock_decoder = start_pipe.value.:(0) &: starter.value
        ; idct = start_pipe.value.:(1) &: starter.value
        ; output = start_pipe.value.:(2) &: starter.value
        }
    }
  ;;

  let hierarchical scope =
    let module Hier = Hierarchy.In_scope (I) (O) in
    Hier.hierarchical ~scope ~name:"ctrl" create
  ;;
end
