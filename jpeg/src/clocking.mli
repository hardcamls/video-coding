open! Base
open! Hardcaml

type 'a t =
  { clock : 'a
  ; clear : 'a
  }
[@@deriving sexp_of, hardcaml]

val to_spec : ?clear_to:Signal.t -> Signal.t t -> Reg_spec.t
val reg : ?enable:Signal.t -> ?clear_to:Signal.t -> Signal.t t -> Signal.t -> Signal.t

val reg_fb
  :  ?enable:Signal.t
  -> ?clear_to:Signal.t
  -> Signal.t t
  -> width:int
  -> f:(Signal.t -> Signal.t)
  -> Signal.t

val state_machine
  :  ?encoding:Always.State_machine.Encoding.t
  -> ?enable:Signal.t
  -> (module Always.State_machine.State with type t = 'a)
  -> Signal.t t
  -> 'a Always.State_machine.t

module Var : sig
  val reg
    :  ?enable:Signal.t
    -> ?clear_to:Signal.t
    -> width:int
    -> Signal.t t
    -> Always.Variable.t
end
