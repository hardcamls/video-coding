val test
  :  ?waves:bool
  -> ?error_tolerance:int
  -> ?num_blocks_to_decode:int
  -> string
  -> Hardcaml_jpeg_model.Frame.t * Hardcaml_waveterm.Waveform.t option
