open! Core

module Make (Config : sig
  include Hardcaml_jpeg.Dct.Config

  val test_range : int
end) : sig
  module Dct : module type of Hardcaml_jpeg.Dct.Make (Config)

  val create_inputs : unit -> int array array
  val reference : int array array -> int array array * int array array
  val print_reference : int array array -> unit
  val simulate_dct : int array array -> int array array * int array array

  val simulate_dct_waveform
    :  ?verbose:bool
    -> int array array
    -> Hardcaml_waveterm.Waveform.t
end
