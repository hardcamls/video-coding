open Core

val marker_length : String.t -> pos:int -> int
val find_next_marker : start_pos:int -> marker_code:int -> String.t -> int option
val find_next_marker_exn : start_pos:int -> marker_code:int -> String.t -> int
val extract_next_marker : start_pos:int -> marker_code:int -> String.t -> String.t option
val extract_next_marker_exn : start_pos:int -> marker_code:int -> String.t -> String.t
val find_nth_marker_exn : n:int -> marker_code:int -> String.t -> String.t

(** Decode JPEG header and return entropy coded segment processed to remove 
    stuffed bytes. *)
val headers_and_entropy_coded_segment
  :  string
  -> Hardcaml_jpeg_model.Decoder.Header.t * String.t

val remove_stuffing_bytes : String.t -> String.t
val mouse480 : string
val load_jpeg_file : string -> Hardcaml_jpeg_model.Bitstream_reader.From_string.t

(** {2 Easy to use bitstream reader module for testbenches.}*)

open Hardcaml
open Hardcaml_jpeg

module Super_simple_bitstream_reader : sig
  module I : sig
    type 'a t =
      { clocking : 'a Clocking.t
      ; read_bits : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t = { bits : 'a } [@@deriving sexp_of, hardcaml]
  end

  val create : bits:String.t -> Scope.t -> Interface.Create_fn(I)(O).t
  val hierarchical : bits:String.t -> Scope.t -> Interface.Create_fn(I)(O).t
end

module Wrapped_marker_decoder (Fields : Interface.S) (D : Fields_decoder.M(Fields).S) : sig
  module I : sig
    type 'a t =
      { clocking : 'a Clocking.t
      ; start : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { fields : 'a Fields.t
      ; done_ : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  val create : bits:String.t -> Scope.t -> Interface.Create_fn(I)(O).t

  val test
    :  ?waves:bool
    -> ?on_cycle:(Bits.t ref O.t -> unit)
    -> String.t
    -> Hardcaml_waveterm.Waveform.t option
end

val load_huffman_tables
  :  cycle:(unit -> unit)
  -> Bits.t ref Hardcaml_jpeg.Markers.Dht.Fields.t
  -> Hardcaml_jpeg_model.Markers.Dht.t list
  -> unit

val load_quant_tables
  :  cycle:(unit -> unit)
  -> Bits.t ref Hardcaml_jpeg.Markers.Dqt.Fields.t
  -> Hardcaml_jpeg_model.Markers.Dqt.t list
  -> unit

val reconstruct : block_number:int -> Hardcaml_jpeg_model.Frame.t -> int array -> unit
