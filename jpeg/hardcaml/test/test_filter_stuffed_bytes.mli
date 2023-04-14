open! Core
open Hardcaml_waveterm

val regression : ?waves:bool -> ?verbose:bool -> ?min:int -> int -> Waveform.t option
