open! Base
open! Hardcaml

type 'a t =
  { clock : 'a
  ; clear : 'a
  }
[@@deriving sexp_of, hardcaml]

let to_spec ?clear_to (i : Signal.t t) =
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  match clear_to with
  | Some clear_to -> Reg_spec.override ~clear_to spec
  | None -> spec
;;

let to_spec_no_clear (i : Signal.t t) = Reg_spec.create ~clock:i.clock ()

let reg ?(enable = Signal.vdd) ?clear_to clocking d =
  Signal.reg (to_spec ?clear_to clocking) ~enable d
;;

let reg_fb ?(enable = Signal.vdd) ?clear_to clocking ~width ~f =
  Signal.reg_fb (to_spec ?clear_to clocking) ~enable ~width ~f
;;

let pipeline ?enable clocking ~n d = Signal.pipeline (to_spec clocking) ?enable ~n d

let state_machine ?encoding ?(enable = Signal.vdd) state clocking =
  Always.State_machine.create ?encoding state (to_spec clocking) ~enable
;;

module Var = struct
  let reg ?(enable = Signal.vdd) ?clear_to ~width clocking =
    let spec = to_spec ?clear_to clocking in
    Always.Variable.reg ~enable ~width spec
  ;;
end
