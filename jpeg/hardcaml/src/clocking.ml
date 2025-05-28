open! Base
open! Hardcaml

type 'a t =
  { clock : 'a
  ; clear : 'a
  }
[@@deriving sexp_of, hardcaml]

let to_spec (i : Signal.t t) = Signal.Reg_spec.create ~clock:i.clock ~clear:i.clear ()
let to_spec_no_clear (i : Signal.t t) = Signal.Reg_spec.create ~clock:i.clock ()

let reg ?(enable = Signal.vdd) ?clear_to clocking d =
  Signal.reg (to_spec clocking) ?clear_to ~enable d
;;

let reg_fb ?(enable = Signal.vdd) ?clear_to clocking ~width ~f =
  Signal.reg_fb (to_spec clocking) ?clear_to ~enable ~width ~f
;;

let pipeline ?enable clocking ~n d = Signal.pipeline (to_spec clocking) ?enable ~n d

let state_machine ?encoding ?(enable = Signal.vdd) state clocking =
  Always.State_machine.create ?encoding state (to_spec clocking) ~enable
;;

module Var = struct
  let reg ?(enable = Signal.vdd) ?clear_to ~width clocking =
    let spec = to_spec clocking in
    Always.Variable.reg ?clear_to ~enable ~width spec
  ;;
end
