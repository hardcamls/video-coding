(** JPEG decoding model. *)

open! Base
module Bits = Bitstream_reader.From_string

module Header : sig
  type t [@@deriving sexp_of]

  (** Decode the header up to a start of scan marker.  After that 
      comes the entropy coded segment. *)
  val decode : Bits.t -> t

  (** {2 Markers decoded from the JPEG header} *)

  val frame : t -> Markers.Sof.t option
  val scan : t -> Markers.Sos.t option
  val quant_tables : t -> Markers.Dqt.t list
  val restart_interval : t -> Markers.Dri.t option
  val huffman_tables : t -> Markers.Dht.t list
end

module Component : sig
  type t [@@deriving sexp_of]

  module Summary : sig
    type coef_block = int array [@@deriving sexp_of]
    type pixel_block = int array [@@deriving sexp_of]
    type nonrec t = t [@@deriving sexp_of]
  end

  val coefs : t -> int array
  val dequant : t -> int array
  val recon : t -> int array
end

type t

val init : Header.t -> Bits.t -> t
val decode : t -> unit
val get_frame : t -> Frame.t
val entropy_coded_bits : t -> Bits.t

(** Decode frame data.  Bits should aligned to the entropy coded segment. *)
val decode_a_frame : Bits.t -> Frame.t

module For_testing : sig
  val mag : int -> int -> int

  (** Given a bitstream that has decoded the headers up to start of scan, 
    extract the entropy coded bits while removing 'stuffed' bytes. *)
  val extract_entropy_coded_bits : Bits.t -> Bits.t

  module Sequenced : sig
    (* Take care!  Lots of stuff happens under the hood when you access this sequence!

       The sequence is memoized to help avoid repeated evaluations, but must be queried 
       sequentially regardless.

       Each time you take the head of this sequence, you will decode a block and
       can go and query the internals on the codec.  This is useful in the hardware 
       testbenches where we want to do detailed checking.

       The component just decoded is returned.
    *)
    val decode : t -> Component.t Sequence.t
  end
end
