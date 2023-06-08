open Base
open Hardcaml
open Signal

module Rle = struct
  type 'a t =
    { run : 'a [@bits 6]
    ; coef : 'a [@bits 12]
    ; last : 'a
    ; dc : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module Rle_out = With_valid.Wrap.Make (Rle)

module I = struct
  type 'a t =
    { clocking : 'a Clocking.t
    ; start : 'a
    ; coef : 'a [@bits 12] [@rtlsuffix "_in"]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { rle_out : 'a Rle_out.t
    ; quant_address : 'a [@bits 6]
    ; quant_read : 'a
    ; done_ : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module State = struct
  type t =
    | Start
    | Preroll_quant
    | Dc
    | Dc_wait
    | Ac
    | Ac_wait
  [@@deriving sexp_of, compare, enumerate]
end

module Var = Always.Variable

let create scope (i : _ I.t) =
  let ( -- ) = Scope.naming scope in
  let sm = Always.State_machine.create (module State) (Clocking.to_spec i.clocking) in
  ignore (sm.current -- "STATE" : Signal.t);
  let read_enable = Var.wire ~default:gnd in
  let address = Clocking.Var.reg i.clocking ~width:7 in
  let address_next = address.value +:. 1 in
  let last = address.value ==:. 64 + Quant.pipeline_depth - 1 in
  let run = Clocking.Var.reg i.clocking ~width:6 in
  let coef_is_zero = i.coef ==:. 0 in
  let run_coef_write = Var.wire ~default:gnd in
  Always.(
    compile
      [ sm.switch
          [ Start, [ run <--. 0; when_ i.start [ sm.set_next Preroll_quant ] ]
          ; ( Preroll_quant
            , [ read_enable <--. 1
              ; address <-- address_next
              ; when_ (address.value ==:. Quant.pipeline_depth - 1) [ sm.set_next Dc ]
              ] )
          ; ( Dc
            , [ read_enable <--. 1
              ; run_coef_write <-- vdd
              ; address <-- address_next
              ; sm.set_next Dc_wait
              ] )
          ; Dc_wait, [ sm.set_next Ac ]
          ; ( Ac
            , [ read_enable <-- (address.value <:. 64)
              ; address <-- address_next
              ; if_
                  coef_is_zero
                  [ run <-- run.value +:. 1 ]
                  [ run_coef_write <-- vdd; run <--. 0 ]
              ; when_ last [ run_coef_write <-- vdd; sm.set_next Start ]
              ] )
          ; Ac_wait, []
          ]
      ]);
  let reg = Clocking.reg ~enable:run_coef_write.value i.clocking in
  { O.rle_out =
      { value =
          { run = reg run.value; coef = reg i.coef; last = reg last; dc = reg (sm.is Dc) }
      ; valid = Clocking.reg i.clocking run_coef_write.value
      }
  ; quant_address = address.value.:[5, 0]
  ; quant_read = read_enable.value
  ; done_ = sm.is Start
  }
;;

let hierarchical scope =
  let module Hier = Hierarchy.In_scope (I) (O) in
  Hier.hierarchical ~scope ~name:"rle" create
;;
