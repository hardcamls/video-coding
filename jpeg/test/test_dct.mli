open! Core

module Make (Config : Hardcaml_jpeg.Dct.Config) : sig
  module Dct : module type of Hardcaml_jpeg.Dct.Make (Config)

  val create_inputs : unit -> int array array
  val reference : int array array -> unit
  val simulate_dct : int array array -> Hardcaml_waveterm.Waveform.t
end
