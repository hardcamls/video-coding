open! Core

val load_jpeg : unit -> String.t
val test_vld : ?waves:bool -> String.t -> Hardcaml_waveterm.Waveform.t option
