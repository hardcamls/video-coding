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

  The central part uses a 40 bits shift register that shifts up in inrements of 8 or 16 bits.

  We use the top 24 (actually 23) bits to hold the (shifted) 16 bit output.  So we need
  to shift the output by 0..7 bits.

  16 bit input data enters at the bottom 24 bits (though at a byte boundary) depending 
  the current shift offset.
*)

module I = struct
  type 'a t =
    { clocking : 'a Clocking.t
    ; start : 'a
    ; read_bits : 'a [@bits 5]
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

module Var = Always.Variable

module State = struct
  type t =
    | Start
    | Sync
    | Pass
    | Load
  [@@deriving sexp_of, compare, enumerate]
end

type reg_with_next =
  { reg : Var.t
  ; next : Var.t
  }

let create scope (i : _ I.t) =
  let ( -- ) = Scope.naming scope in
  let vname (v : Var.t) name = ignore (v.value -- name : Signal.t) in
  let reg_with_next clocking ~name ~width =
    let reg = Clocking.Var.reg clocking ~width in
    let next = Var.wire ~default:reg.value in
    ignore (reg.value -- name : Signal.t);
    ignore (next.value -- (name ^ "_next") : Signal.t);
    { reg; next }
  in
  let sm = Always.State_machine.create (module State) (Clocking.to_spec i.clocking) in
  ignore (sm.current -- "STATE" : Signal.t);
  let buffer = reg_with_next i.clocking ~name:"buffer" ~width:40 in
  let shift_offset = Clocking.Var.reg i.clocking ~width:3 in
  vname shift_offset "shift_offset";
  let shift_offset_next = reg_with_next i.clocking ~name:"shift_offset_next" ~width:5 in
  let bits = log_shift sll (sel_top buffer.reg.value 24) shift_offset.value in
  let one_byte_shifted = reg_with_next i.clocking ~name:"one_byte_shifted" ~width:1 in
  let read = Var.wire ~default:gnd in
  let jpeg_ready = Var.wire ~default:gnd in
  let bits_valid = Var.wire ~default:gnd in
  let update_state = Var.wire ~default:gnd in
  let sync_count = Clocking.Var.reg i.clocking ~width:1 in
  (* combinational shift register logic *)
  Always.(
    compile
      [ (* combinationally work out the next shift register state *)
        shift_offset_next.next <-- uresize shift_offset.value 5 +: i.read_bits
      ; if_
          shift_offset_next.next.value.:(4)
          [ (* >= 16 *)
            read <-- vdd
          ; if_
              one_byte_shifted.reg.value
              [ buffer.next
                <-- drop_bottom (drop_top buffer.reg.value 16) 8 @: i.jpeg_in @: zero 8
              ]
              [ buffer.next <-- drop_top buffer.reg.value 16 @: i.jpeg_in ]
          ]
        @@ elif
             shift_offset_next.next.value.:(3)
             [ (* >= 8 *)
               if_
                 one_byte_shifted.reg.value
                 [ buffer.next
                   <-- drop_bottom (drop_top buffer.reg.value 8) 8 @: i.jpeg_in
                 ; one_byte_shifted.next <-- gnd
                 ; read <-- vdd
                 ]
                 [ buffer.next <-- drop_top buffer.reg.value 8 @: zero 8
                 ; one_byte_shifted.next <-- vdd
                 ]
             ]
             []
      ; (* Control updating the state based on validity of input data. *)
        sm.switch
          [ ( Start
            , [ sync_count <--. 0
              ; shift_offset <--. 0
              ; one_byte_shifted.reg <-- vdd
              ; when_ i.start [ sm.set_next Sync ]
              ] )
          ; ( Sync
            , [ jpeg_ready <-- vdd
              ; when_
                  i.jpeg_valid
                  [ sync_count <-- sync_count.value +:. 1
                  ; buffer.reg
                    <-- drop_bottom (drop_top buffer.reg.value 16) 8
                        @: i.jpeg_in
                        @: zero 8
                  ; when_ (sync_count.value ==:. 1) [ sm.set_next Pass ]
                  ]
              ] )
          ; ( Pass
            , [ bits_valid <-- vdd
              ; jpeg_ready <-- read.value
              ; if_
                  read.value
                  [ if_
                      i.jpeg_valid
                      [ update_state <-- vdd ]
                      [ shift_offset_next.reg <-- shift_offset_next.next.value
                      ; sm.set_next Load
                      ]
                  ]
                  [ update_state <-- vdd ]
              ] )
          ; ( Load
            , [ jpeg_ready <-- vdd
              ; bits_valid <-- gnd
              ; (* output the correct shift value given the previous amount to be shift by *)
                shift_offset_next.next <-- shift_offset_next.reg.value
              ; when_ i.jpeg_valid [ update_state <-- vdd; sm.set_next Pass ]
              ] )
          ]
      ; (* Perform the state update *)
        when_
          update_state.value
          [ buffer.reg <-- buffer.next.value
          ; one_byte_shifted.reg <-- one_byte_shifted.next.value
          ; shift_offset <-- shift_offset_next.next.value.:[2, 0]
          ]
      ]);
  { O.bits = sel_top bits 16
  ; bits_valid = bits_valid.value
  ; jpeg_ready = jpeg_ready.value
  }
;;

let hierarchical scope =
  let module Hier = Hierarchy.In_scope (I) (O) in
  Hier.hierarchical ~scope ~name:"bsread" create
;;

module Full = struct
  module I = struct
    type 'a t =
      { clocking : 'a Clocking.t
      ; start : 'a
      ; header_or_entropy : 'a
      ; read_byte : 'a
      ; read_bits : 'a [@bits 5]
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
      ; byte : 'a [@bits 8]
      ; byte_valid : 'a
      ; jpeg_ready : 'a (** Core reads the next 16 bits in *)
      }
    [@@deriving sexp_of, hardcaml]
  end

  module State = struct
    type t =
      | Start
      | Header_mode
      | Entropy_mode
      | Resync
    [@@deriving sexp_of, compare, enumerate]
  end

  let _create _scope (i : _ I.t) =
    let sm = Always.State_machine.create (module State) (Clocking.to_spec i.clocking) in
    let byte_offset = Clocking.Var.reg i.clocking ~width:1 in
    let byte_valid = Var.wire ~default:gnd in
    let byte_out = Var.wire ~default:(zero 8) in
    Always.(
      compile
        [ sm.switch
            [ Start, [ byte_offset <-- gnd; when_ i.start [ sm.set_next Header_mode ] ]
            ; ( Header_mode
              , [ when_
                    i.jpeg_valid
                    [ byte_valid <-- vdd
                    ; byte_out
                      <-- mux2 byte_offset.value i.jpeg_in.:[7, 0] i.jpeg_in.:[15, 8]
                    ]
                ] )
            ; ( Entropy_mode
              , [ (* Track the input looking for the EOI marker.  
                     When we see it, we will suppress the input to the entropy bits reader. *) ]
              )
            ; Resync, []
            ]
        ]);
    O.Of_signal.of_int 0
  ;;
end
