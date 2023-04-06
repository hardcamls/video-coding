(** JPEG decoding model. *)

open! Base
module Bits = Bitstream_reader.From_string

module Header : sig
  type t [@@deriving sexp_of]

  val empty : t

  (** Decode the header up to a start of scan marker.  After that 
      comes the entropy coded segment. *)
  val decode : Bits.t -> t -> t

  (** {2 Markers decoded from the JPEG header} *)

  val frame : t -> Markers.Sof.t option
  val scan : t -> Markers.Sos.t option
  val quant_tables : t -> Markers.Dqt.t list
  val restart_interval : t -> Markers.Dri.t option
  val huffman_tables : t -> Markers.Dht.t list
end

(** Decode a frame and it's headers. *)
val decode_frame : Bits.t -> Plane.t array

module For_testing : sig
  val mag : int -> int -> int

  (** Given a bitstream that has decoded the headers up to start of scan, 
    extract the entropy coded bits while removing 'stuffed' bytes. *)
  val extract_entropy_coded_bits : Bits.t -> Bits.t
end
