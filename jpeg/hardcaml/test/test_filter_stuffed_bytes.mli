open! Core
open Hardcaml_waveterm

val display_rules : Display_rules.t
val regression : ?waves:bool -> ?verbose:bool -> ?min:int -> int -> Waveform.t option
