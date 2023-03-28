open! Base
open Hardcaml
open Signal

module I = struct
  type 'a t =
    { clocking : 'a Clocking.t
    ; advance_bits : 'a [@bits 5]
          (** Advance the bitstream by 0 to 16 bits. Values > 16 lead to undefined
              behaviour.*)
    ; bits_in : 'a [@bits 16]
          (** 32 bits of data from the bitstream provided to the reader. *)
    ; bits_in_available : 'a (** The next 16 bits of input bitstream are available *)
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { bits : 'a [@bits 16] (** The next 16 bits of the bitstream*)
    ; bits_out_available : 'a (** [bits] has data available *)
    ; read_bits_in : 'a (** Core wants to read the next 32 bits in *)
    }
  [@@deriving sexp_of, hardcaml]
end

let or_0 s a = mux2 s a (zero (width a))

(* We maintain a 48 bit internal buffer where upto 16 bits can be input and
   output per cycle. Note that a 32 bit buffer doesn't work so well as it stalls
   a lot. *)
let create _scope (i : _ I.t) =
  (* Add and/or subtract bits from the buffer *)
  let bits_available = wire 6 in
  (* Bits are available at the output *)
  let can_output_bits = bits_available >=:. 16 in
  (* Space to read input bits *)
  let can_input_bits = bits_available <=:. 32 in
  (* space to read inputs bits and bits are available *)
  let reading_bits_in = can_input_bits &: i.bits_in_available in
  bits_available
  <== Clocking.reg_fb i.clocking ~enable:vdd ~width:6 ~f:(fun d ->
          Uop.(
            d
            -: or_0 can_output_bits i.advance_bits
            +: or_0 reading_bits_in (of_int ~width:5 16)).:[5, 0])
      -- "num_bits_available";
  let buffer =
    Clocking.reg_fb i.clocking ~enable:reading_bits_in ~width:48 ~f:(fun d ->
        i.bits_in @: d.:[47, 16])
    -- "bits_buffer"
  in
  let bits_buffer_offset =
    (of_int ~width:6 48 -: bits_available).:[5, 0] -- "bits_buffer_offset"
  in
  let bits =
    mux bits_buffer_offset (List.init 33 ~f:(fun i -> (srl buffer i).:[15, 0]))
  in
  { O.bits; bits_out_available = can_output_bits; read_bits_in = can_input_bits }
;;

let hierarchical scope =
  let module Hier = Hierarchy.In_scope (I) (O) in
  Hier.hierarchical ~scope ~name:"reader" create
;;
