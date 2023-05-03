val test
  :  ?waves:bool
  -> ?error_tolerance:int
  -> ?num_blocks_to_decode:int
  -> string
  -> Hardcaml_video_common.Frame.t * Hardcaml_waveterm.Waveform.t option
