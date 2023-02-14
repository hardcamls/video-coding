open! Core

val create_inputs : unit -> int array array
val reference : int array array -> unit
val simulate_idct : int array array -> Hardcaml_waveterm.Waveform.t
