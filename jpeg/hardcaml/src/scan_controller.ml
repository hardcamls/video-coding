open Base
open Hardcaml
open Signal

module Ctrl = struct
  type 'a t =
    { codeblock_decoder : 'a
    ; idct : 'a
    ; output : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module I = struct
  type 'a t =
    { clocking : 'a Clocking.t
    ; start : 'a
    ; done_ : 'a Ctrl.t [@rtlsuffix "_done"]
    ; dc_pred_in : 'a [@bits 12]
    ; dc_pred_write : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { start : 'a Ctrl.t [@rtlsuffix "_start"]
    ; done_ : 'a
    ; dc_pred_out : 'a [@bits 12]
    ; luma_or_chroma : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module State = struct
  type t =
    | Start
    | Y
    | Cb
    | Cr
  [@@deriving sexp_of, compare, enumerate]
end

module Var = Always.Variable

(* XXX cheat to start with and assume a simple yuv420 encoding in the reasonable 
   scan order.  We'll generalize later.*)

let create scope (i : _ I.t) =
  let ( -- ) = Scope.naming scope in
  let sm = Always.State_machine.create (module State) (Clocking.to_spec i.clocking) in
  ignore (sm.current -- "STATE" : Signal.t);
  (* let starts = Ctrl.Of_always.wire zero in
  let all_done = reduce ~f:( &: ) (Ctrl.to_list i.done_) in *)
  let starts = Ctrl.Of_always.reg (Clocking.to_spec i.clocking) in
  let all_done =
    let dones = reduce ~f:( &: ) (Ctrl.to_list i.done_) in
    let starts = reduce ~f:( |: ) (Ctrl.to_list (Ctrl.Of_always.value starts)) in
    dones &: ~:starts
  in
  let component = Clocking.Var.reg i.clocking ~width:2 in
  let x_blk = Clocking.Var.reg i.clocking ~width:1 in
  let y_blk = Clocking.Var.reg i.clocking ~width:1 in
  let x = Clocking.Var.reg i.clocking ~width:16 in
  let y = Clocking.Var.reg i.clocking ~width:16 in
  let start_pipe = Clocking.Var.reg i.clocking ~width:2 in
  let dc_y_pred = Clocking.Var.reg i.clocking ~width:12 in
  let dc_cb_pred = Clocking.Var.reg i.clocking ~width:12 in
  let dc_cr_pred = Clocking.Var.reg i.clocking ~width:12 in
  let luma_or_chroma = Clocking.Var.reg i.clocking ~width:1 in
  Always.(
    compile
      [ Ctrl.Of_always.assign starts (Ctrl.Of_signal.of_int 0)
      ; sm.switch
          [ ( Start
            , [ component <--. 0
              ; x <--. 0
              ; y <--. 0
              ; x_blk <--. 0
              ; y_blk <--. 0
              ; when_
                  i.start
                  [ start_pipe <--. 1
                  ; starts.codeblock_decoder <-- vdd
                  ; luma_or_chroma <-- gnd
                  ; sm.set_next Y
                  ]
              ] )
          ; ( Y
            , [ when_ i.dc_pred_write [ dc_y_pred <-- i.dc_pred_in ]
              ; when_
                  all_done
                  [ (* XX this assumes the start pipe flushes during the initial luma phase, 
                         which might not be true for 444 images.  I think we can do something smarter 
                         in the Ctrl module as the operation is ia pretty simple shift register thing *)
                    start_pipe <-- lsbs start_pipe.value @: vdd
                  ; starts.codeblock_decoder <-- vdd
                  ; starts.idct <-- start_pipe.value.:(0)
                  ; starts.output <-- start_pipe.value.:(1)
                  ; x_blk <-- x_blk.value +:. 1
                  ; when_
                      (x_blk.value ==:. 1)
                      [ x_blk <--. 0
                      ; y_blk <-- y_blk.value +:. 1
                      ; when_
                          (y_blk.value ==:. 1)
                          [ luma_or_chroma <-- vdd; sm.set_next Cb ]
                      ]
                  ]
              ] )
          ; ( Cb
            , [ when_ i.dc_pred_write [ dc_cb_pred <-- i.dc_pred_in ]
              ; when_
                  all_done
                  [ starts.codeblock_decoder <-- vdd
                  ; starts.idct <-- vdd
                  ; starts.output <-- vdd
                  ; sm.set_next Cr
                  ]
              ] )
          ; ( Cr
            , [ when_ i.dc_pred_write [ dc_cr_pred <-- i.dc_pred_in ]
              ; when_
                  all_done
                  [ starts.codeblock_decoder <-- vdd
                  ; starts.idct <-- vdd
                  ; starts.output <-- vdd
                  ; luma_or_chroma <-- gnd
                  ; sm.set_next Y
                  ; x <-- x.value +:. 1
                  ; when_
                      (x.value ==:. (480 / 8) - 1)
                      [ x <--. 0
                      ; y <-- y.value +:. 1
                      ; when_
                          (y.value ==:. (320 / 8) - 1)
                          [ starts.codeblock_decoder <-- gnd
                          ; starts.idct <-- gnd
                          ; starts.output <-- gnd
                          ; y <--. 0
                          ; sm.set_next Start
                          ]
                      ]
                  ]
              ] )
          ]
      ]);
  { O.start = Ctrl.Of_always.value starts
  ; done_ = sm.is Start
  ; dc_pred_out =
      mux2 (sm.is Cb) dc_cb_pred.value (mux2 (sm.is Cr) dc_cr_pred.value dc_y_pred.value)
  ; luma_or_chroma = luma_or_chroma.value
  }
;;

let hierarchical scope =
  let module Hier = Hierarchy.In_scope (I) (O) in
  Hier.hierarchical ~scope ~name:"ctrl" create
;;

module New = struct
  module I = struct
    type 'a t =
      { clocking : 'a Clocking.t
      ; start : 'a
      ; sof : 'a Markers.Sof.Fields.t [@rtlprefix "sof$"]
      ; sos : 'a Markers.Sos.Fields.t [@rtlprefix "sos$"]
      ; done_ : 'a Ctrl.t [@rtlsuffix "_done"]
      ; dc_pred_in : 'a [@bits 12]
      ; dc_pred_write : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { start : 'a Ctrl.t [@rtlsuffix "_start"]
      ; done_ : 'a
      ; dc_pred_out : 'a [@bits 12]
      ; luma_or_chroma : 'a
      ; x_pos : 'a [@bits 16]
      ; y_pos : 'a [@bits 16]
      }
    [@@deriving sexp_of, hardcaml]
  end

  (* So, in the sof we have an identifier which we cannot control, but there will 
     be a limited number of them in total (4 seems enough).

     We need to map the indexes to a simpler linear RAM address. 

     In the scan header we refer back to the frame component identifier, and must map 
     that back to the ram index.
  *)

  let max_components = 4
  let log_max_components = Int.ceil_log2 max_components
  let max_scans = 4
  let log_max_scans = Int.ceil_log2 max_scans

  (* 1 for baseline, 2 for extended+progressive *)
  let log_max_coef_tables = 1

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
      | Component
      | Blocks
    [@@deriving sexp_of, compare, enumerate, variants]

    let strings = List.map all ~f:(fun v -> Sexp.to_string (sexp_of_t v))
  end

  module Scan_state = struct
    type 'a t =
      { dc : 'a [@bits log_max_coef_tables]
      ; ac : 'a [@bits log_max_coef_tables]
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
                         { dc = i.sos.scan_selector.fields.dc_coef_selector
                         ; ac = i.sos.scan_selector.fields.ac_coef_selector
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
    (* 
      foreach scan_copmonent {
        for vert_sampling {
          for horz_sampling {
            decode a block
          }
        }
      }   
    *)
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
    Always.(
      compile
        [ update_scan <-- gnd
        ; sm.switch
            [ ( Headers
              , [ when_
                    i.sos.write_scan_selector
                    [ scan_address <-- lsbs scan_address_next ]
                ; when_ i.start [ scan_address <--. 0; sm.set_next Component ]
                ] )
            ; ( Component
              , [ component_address <-- scan.component_index
                ; Scan_state.Of_always.assign scan_r scan
                ; x_pos_start <-- scan.x_pos
                ; y_pos_start <-- scan.y_pos
                ; blk_x <--. 0
                ; blk_y <--. 0
                ; sm.set_next Blocks
                ] )
            ; ( Blocks
              , [ component_address <-- scan_r.component_index.value
                ; when_
                    vdd
                    [ blk_x <-- blk_x_next
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
                            ; blk_y <--. 0
                            ; scan_r.x_pos <-- x_pos_next
                            ; scan_r.y_pos <-- y_pos_start.value
                            ; when_
                                (x_pos_next ==: component_width)
                                [ scan_r.x_pos <--. 0; scan_r.y_pos <-- y_pos_next ]
                            ; sm.set_next Component
                            ; when_
                                (last_component &: last_col &: last_row)
                                [ sm.set_next Headers ]
                            ]
                        ]
                    ]
                ] )
            ]
        ]);
    { O.start = { codeblock_decoder = gnd; idct = gnd; output = gnd }
    ; done_ = gnd
    ; dc_pred_out = zero 12
    ; luma_or_chroma = gnd
    ; x_pos = scan_r.x_pos.value @: zero 3
    ; y_pos = scan_r.y_pos.value @: zero 3
    }
  ;;
end
