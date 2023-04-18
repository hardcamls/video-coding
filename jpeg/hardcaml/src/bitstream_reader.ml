open! Base
open Hardcaml
open Signal

(* We have the following requirements for this module. 
   
   1. Roughly 16 bits per cycle IO - this is needed to keep up with the variable length
      decoding process.  At input we'll take a 16 bit AXI-stream like interface.
   2. Two modes of operation - header parsing and entropy parsing. 
   3. During header parsing, we do not need to preprocess the data, and will only read 
      8 (or 0) bits per cycle. 
   4. During entropy parsing ... 
   5. 0..16 bits per cycle 
   6. Stuffed 0x00 bytes after 0xFF bytes should be removed 
   7. We can finish the entropy segment if we see 0xFF followed by a non-zero.  Presumably it 
      should be EOI.  
   8. We might want to output the trailing header to flush the entropy segment.
*)

module I = struct
  type 'a t =
    { clocking : 'a Clocking.t
    ; header_or_entropy_mode : 'a (** 1 for header mode, 0 for entropy mode *)
    ; read_header_byte : 'a
    ; read_entropy_bits : 'a [@bits 5]
          (** Advance the bitstream by 0 to 16 bits. Values > 16 lead to undefined
              behaviour.*)
    ; jpeg_in : 'a [@bits 16]
          (** 32 bits of data from the bitstream provided to the reader. *)
    ; jpeg_valid : 'a (** The next 16 bits of input bitstream are available *)
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { bits : 'a [@bits 16] (** The next 16 bits of the bitstream*)
    ; bits_valid : 'a (** [bits] has 16 bits of data available *)
    ; jpeg_ready : 'a (** Core reads the next 16 bits in *)
    }
  [@@deriving sexp_of, hardcaml]
end

(* Another way of thinking about this. 
   
  We have a 40 bit shift register formed of 5 bytes 

  Data enters at bytes [3][4].  No shifting required, but we will need to route 
  bytes to the correct place.  If we're removing stuffing bytes, we might need 
  to only write to one or the other location.

  We have an 8 bit shift window out from bytes [0][1][2].  As we read we shift down by 1 or 2
  places and update the shift offset.

  [0][1][2][3][4]

  Although no shifting on the output would be nice, 8 places (hopefully we only need 0..7) should
  be relatively light - perhaps just 1 or 2 luts, which is much better than the current design.
*)

module Var = Always.Variable

let create scope (i : _ I.t) =
  let ( -- ) = Scope.naming scope in
  let vname (v : Var.t) name = ignore (v.value -- name : Signal.t) in
  let buffer = Clocking.Var.reg i.clocking ~width:40 in
  vname buffer "buffer";
  let shift_offset = Clocking.Var.reg i.clocking ~width:3 in
  vname shift_offset "shift_offset";
  let shift_offset_next =
    uresize shift_offset.value 5 +: i.read_entropy_bits -- "shift_offset_next"
  in
  let bits = log_shift sll (sel_top buffer.value 24) shift_offset.value in
  let one_byte_shifted = Clocking.Var.reg i.clocking ~width:1 in
  vname one_byte_shifted "one_byte_shifted";
  let jpeg_ready = Var.wire ~default:gnd in
  let _bits_valid = Var.wire ~default:gnd in
  Always.(
    compile
      [ shift_offset <-- shift_offset_next.:[2, 0]
      ; if_
          shift_offset_next.:(4)
          [ (* >= 16 *)
            jpeg_ready <-- vdd
          ; if_
              one_byte_shifted.value
              [ buffer <-- drop_bottom (drop_top buffer.value 16) 8 @: i.jpeg_in @: zero 8
              ]
              [ buffer <-- drop_top buffer.value 16 @: i.jpeg_in ]
          ]
        @@ elif
             shift_offset_next.:(3)
             [ (* >= 8 *)
               if_
                 one_byte_shifted.value
                 [ buffer <-- drop_bottom (drop_top buffer.value 8) 8 @: i.jpeg_in
                 ; one_byte_shifted <-- gnd
                 ; jpeg_ready <-- vdd
                 ]
                 [ buffer <-- drop_top buffer.value 8 @: zero 8
                 ; one_byte_shifted <-- vdd
                 ]
             ]
             []
      ]);
  { O.bits = sel_top bits 16; bits_valid = gnd; jpeg_ready = jpeg_ready.value }
;;
