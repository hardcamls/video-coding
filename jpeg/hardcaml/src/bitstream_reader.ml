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

let or_0 s a = mux2 s a (zero (width a))

(* We maintain a 48 bit internal buffer where upto 16 bits can be input and
   output per cycle. Note that a 32 bit buffer doesn't work so well as it stalls
   a lot. *)
let create scope (i : _ I.t) =
  let ( -- ) = Scope.naming scope in
  (* Add and/or subtract bits from the buffer *)
  let bits_available = Clocking.Var.reg i.clocking ~width:6 in
  ignore (bits_available.value -- "num_bits_available" : Signal.t);
  (* Bits are available at the output *)
  let can_output_bits = bits_available.value >=:. 16 in
  (* Space to read input bits *)
  let can_input_bits = bits_available.value <=:. 32 in
  (* space to read inputs bits and bits are available *)
  let reading_bits_in = can_input_bits &: i.jpeg_valid in
  let buffer = Clocking.Var.reg i.clocking ~width:48 in
  ignore (buffer.value -- "bits_buffer" : Signal.t);
  Always.(
    compile
      [ if_
          i.header_or_entropy_mode
          [ (* header mode *) bits_available <--. 0 ]
          [ (* entropy mode *)
            bits_available
            <-- Uop.(
                  bits_available.value
                  -: or_0 can_output_bits i.read_entropy_bits
                  +: or_0 reading_bits_in (of_int ~width:5 16)).:[5, 0]
          ; when_ reading_bits_in [ buffer <-- i.jpeg_in @: buffer.value.:[47, 16] ]
          ]
      ]);
  let bits_buffer_offset =
    (of_int ~width:6 48 -: bits_available.value).:[5, 0] -- "bits_buffer_offset"
  in
  let bits =
    mux bits_buffer_offset (List.init 33 ~f:(fun i -> (srl buffer.value i).:[15, 0]))
  in
  { O.bits; bits_valid = can_output_bits; jpeg_ready = can_input_bits }
;;

let hierarchical scope =
  let module Hier = Hierarchy.In_scope (I) (O) in
  Hier.hierarchical ~scope ~name:"reader" create
;;

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

let create_new scope (i : _ I.t) =
  let ( -- ) = Scope.naming scope in
  let vname (v : Var.t) name = ignore (v.value -- name : Signal.t) in
  let bytes = Array.init 5 ~f:(fun _ -> Clocking.Var.reg i.clocking ~width:8) in
  let shift_offset = Clocking.Var.reg i.clocking ~width:3 in
  vname shift_offset "shift_offset";
  let shift_offset_next =
    uresize shift_offset.value 5 +: i.read_entropy_bits -- "shift_offset_next"
  in
  let bits =
    log_shift
      sll
      (bytes.(0).value @: bytes.(1).value @: bytes.(2).value)
      shift_offset.value
  in
  let shift_out_by_1_byte = Var.wire ~default:gnd in
  vname shift_out_by_1_byte "shift_out_by_1_byte";
  let shift_out_by_2_bytes = Var.wire ~default:gnd in
  vname shift_out_by_2_bytes "shift_out_by_2_bytes";
  let load_output_byte index =
    Always.(
      proc
        [ when_ shift_out_by_1_byte.value [ bytes.(index) <-- bytes.(index + 1).value ]
        ; when_ shift_out_by_2_bytes.value [ bytes.(index) <-- bytes.(index + 2).value ]
        ])
  in
  (* Control output *)
  Always.(
    compile
      [ (* control shifting by 1 or 2 bytes into the output registers *)
        proc (List.init 3 ~f:load_output_byte)
      ; (* compute the next shift offset, and shift by bytes as appropriate *)
        shift_offset <-- shift_offset_next.:[2, 0]
      ; if_ shift_offset_next.:(4) [ shift_out_by_2_bytes <-- vdd ]
        @@ elif shift_offset_next.:(3) [ shift_out_by_1_byte <-- vdd ] []
      ]);
  (* Control input. *)
  Always.(compile [ bytes.(3) <-- i.jpeg_in.:[15, 8]; bytes.(4) <-- i.jpeg_in.:[7, 0] ]);
  { (O.Of_signal.of_int 0) with bits = bits.:-[None, 16] }
;;
