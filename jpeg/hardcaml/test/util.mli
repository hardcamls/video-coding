open Core

val marker_length : String.t -> pos:int -> int
val find_next_marker : start_pos:int -> marker_code:int -> String.t -> int option
val find_next_marker_exn : start_pos:int -> marker_code:int -> String.t -> int
val extract_next_marker : start_pos:int -> marker_code:int -> String.t -> String.t option
val extract_next_marker_exn : start_pos:int -> marker_code:int -> String.t -> String.t
