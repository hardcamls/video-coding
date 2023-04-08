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
