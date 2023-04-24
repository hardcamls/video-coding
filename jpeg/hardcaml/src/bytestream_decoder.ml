(* Read the input stream byte-by-byte and decode the various headers and 
   entropy coded segments.
   
   There is a definite limit to doing this a byte at a time as we can consume 
   16 bits per cycle in the huffman decoder.  However, in practice I doubt this 
   is a serioius issue and is /much/ simpler to get a full top level jpeg 
   decoder working.
*)
open! Base
open Hardcaml
open Signal

module I = struct
  type 'a t =
    { clocking : 'a Clocking.t
    ; start : 'a
    ; jpeg : 'a [@bits 8]
    ; jpeg_valid : 'a
    ; bits_ready : 'a
    ; decoder_done : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { jpeg_ready : 'a
    ; done_ : 'a
    ; bits : 'a [@bits 8]
    ; bits_valid : 'a
    ; markers : 'a Markers.All.t
    ; decoder_start : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module State = struct
  type t =
    | Start
    | Scan_for_marker (* Look for a JPEG marker *)
    | Decode_marker (* Decoder which marker *)
    | Wait_for_fields (* Decode specific marker *)
    | Skip_length (* skip marker bytes, based on length field *)
    | Skip
    | Entropy_coded_segment (* output entropy coded segment *)
    | Entropy_marker
    | End_of_image
  [@@deriving sexp_of, compare, enumerate]

  let names = List.map all ~f:(fun v -> sexp_of_t v |> Sexp.to_string_hum)
end

module M = Hardcaml_jpeg_model.Marker_code
module Var = Always.Variable

let markers x = List.map x ~f:(fun (x, y) -> Signal.of_int ~width:8 x, y)

(* SOI -> headers -> SOS -> EOI (repeat) *)

let create scope (i : _ I.t) =
  let ( -- ) = Scope.naming scope in
  let sm = Always.State_machine.create (module State) (Clocking.to_spec i.clocking) in
  let dht = Markers.Dht.O.Of_signal.wires () in
  let start_dht = Var.wire ~default:gnd in
  let dqt = Markers.Dqt.O.Of_signal.wires () in
  let start_dqt = Var.wire ~default:gnd in
  let sof = Markers.Sof.O.Of_signal.wires () in
  let start_sof = Var.wire ~default:gnd in
  let sos = Markers.Sos.O.Of_signal.wires () in
  let start_ecs = Clocking.Var.reg i.clocking ~width:1 in
  let start_sos = Var.wire ~default:gnd in
  let jpeg_ready = Var.wire ~default:gnd in
  let bits_valid = Var.wire ~default:gnd in
  let skip_count = Clocking.Var.reg i.clocking ~width:16 in
  let skip_length = Clocking.Var.reg i.clocking ~width:16 in
  ignore (skip_length.value -- "SKIP_LENGTH" : Signal.t);
  let skip_length_next = skip_length.value.:[7, 0] @: i.jpeg in
  let skip_count_next = skip_count.value +:. 1 in
  ignore (skip_count.value -- "SKIP_COUNT" : Signal.t);
  ignore (sm.current -- "STATE" : Signal.t);
  let decoder_start = Var.wire ~default:gnd in
  Always.(
    compile
      [ start_dht <-- gnd
      ; start_dqt <-- gnd
      ; start_sof <-- gnd
      ; start_sos <-- gnd
      ; sm.switch
          [ Start, [ when_ i.start [ sm.set_next Scan_for_marker ] ]
          ; ( Scan_for_marker
            , [ jpeg_ready <-- vdd
              ; when_ (i.jpeg_valid &: (i.jpeg ==:. 0xff)) [ sm.set_next Decode_marker ]
              ] )
          ; ( Decode_marker
            , [ jpeg_ready <-- vdd
              ; when_
                  i.jpeg_valid
                  [ sm.set_next Skip_length
                  ; switch
                      i.jpeg
                      (markers
                         [ 0xff, [ sm.set_next Decode_marker ]
                         ; M.soi, [ sm.set_next Scan_for_marker ]
                         ; M.eoi, [ sm.set_next Scan_for_marker ]
                         ; M.sof0, [ start_sof <-- vdd; sm.set_next Wait_for_fields ]
                         ; M.dht, [ start_dht <-- vdd; sm.set_next Wait_for_fields ]
                         ; M.dqt, [ start_dqt <-- vdd; sm.set_next Wait_for_fields ]
                         ; ( M.sos
                           , [ start_sos <-- vdd
                             ; start_ecs <-- vdd
                             ; sm.set_next Wait_for_fields
                             ] )
                         ])
                  ]
              ] )
          ; ( Wait_for_fields
            , [ when_
                  (dht.done_ &: dqt.done_ &: sof.done_ &: sos.done_)
                  [ sm.set_next Scan_for_marker
                  ; when_
                      start_ecs.value
                      [ start_ecs <-- gnd
                      ; decoder_start <-- vdd
                      ; sm.set_next Entropy_coded_segment
                      ]
                  ]
              ] )
          ; ( Skip_length
            , [ jpeg_ready <-- vdd
              ; when_
                  i.jpeg_valid
                  [ skip_count <-- skip_count_next
                  ; skip_length <-- skip_length_next
                  ; when_
                      skip_count.value.:(0)
                      [ skip_count <--. 3
                      ; if_
                          (skip_length_next ==:. 2)
                          [ sm.set_next Scan_for_marker ]
                          [ sm.set_next Skip ]
                      ]
                  ]
              ] )
          ; ( Skip
            , [ jpeg_ready <-- vdd
              ; when_
                  i.jpeg_valid
                  [ skip_count <-- skip_count_next
                  ; when_
                      (skip_count.value ==: skip_length.value)
                      [ skip_count <--. 0; sm.set_next Scan_for_marker ]
                  ]
              ] )
          ; ( Entropy_coded_segment
            , [ jpeg_ready <-- gnd
              ; when_
                  (i.jpeg_valid &: i.bits_ready)
                  [ jpeg_ready <-- vdd
                  ; bits_valid <-- i.jpeg_valid
                  ; when_ (i.jpeg ==:. 0xff) [ sm.set_next Entropy_marker ]
                  ]
              ] )
          ; ( Entropy_marker
            , [ jpeg_ready <-- vdd
              ; when_
                  (i.jpeg_valid &: i.bits_ready)
                  [ bits_valid <-- vdd
                  ; sm.set_next Entropy_coded_segment
                  ; when_ (i.jpeg ==:. M.eoi) [ sm.set_next End_of_image ]
                  ; when_ (i.jpeg ==:. 0x0) [ bits_valid <-- gnd ]
                  ]
              ] )
          ; End_of_image, [ (* Wait for decoder to complete *) sm.set_next Start ]
          ]
      ]);
  Markers.Dht.O.Of_signal.assign
    dht
    (Markers.Dht.hierarchical
       scope
       { Markers.Dht.I.clocking = i.clocking
       ; start = start_dht.value
       ; bits = i.jpeg
       ; bits_valid = i.jpeg_valid
       });
  Markers.Dqt.O.Of_signal.assign
    dqt
    (Markers.Dqt.hierarchical
       scope
       { Markers.Dqt.I.clocking = i.clocking
       ; start = start_dqt.value
       ; bits = i.jpeg
       ; bits_valid = i.jpeg_valid
       });
  Markers.Sof.O.Of_signal.assign
    sof
    (Markers.Sof.hierarchical
       scope
       { Markers.Sof.I.clocking = i.clocking
       ; start = start_sof.value
       ; bits = i.jpeg
       ; bits_valid = i.jpeg_valid
       });
  Markers.Sos.O.Of_signal.assign
    sos
    (Markers.Sos.hierarchical
       scope
       { Markers.Sos.I.clocking = i.clocking
       ; start = start_sos.value
       ; bits = i.jpeg
       ; bits_valid = i.jpeg_valid
       });
  { O.jpeg_ready =
      dht.read_bits |: dqt.read_bits |: sof.read_bits |: sos.read_bits |: jpeg_ready.value
  ; done_ = sm.is Start
  ; markers = { dht = dht.fields; dqt = dqt.fields; sof = sof.fields; sos = sos.fields }
  ; bits = i.jpeg
  ; bits_valid = bits_valid.value
  ; decoder_start = decoder_start.value
  }
;;

let hierarchical scope =
  let module Hier = Hierarchy.In_scope (I) (O) in
  Hier.hierarchical ~scope ~name:"bytedec" create
;;

module With_fifo16 = struct
  module O' = O
  module I = I

  module O = struct
    type 'a t =
      { jpeg_ready : 'a
      ; done_ : 'a
      ; bits : 'a [@bits 16]
      ; bits_valid : 'a
      ; markers : 'a Markers.All.t
      ; decoder_start : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  let widen ~clocking ~d ~wr =
    let toggle = Clocking.reg_fb clocking ~enable:wr ~width:1 ~f:(fun d -> ~:d) in
    let d_prev = Clocking.reg clocking ~enable:wr d in
    d_prev @: d, wr &: toggle
  ;;

  let create ~capacity_in_bytes scope (i : _ I.t) =
    let bytestream = O'.Of_signal.wires () in
    let not_full = wire 1 in
    let fifo =
      let d, wr =
        widen
          ~clocking:i.clocking
          ~d:bytestream.bits
          ~wr:(bytestream.bits_valid &: not_full)
      in
      Fifo.create_showahead_with_extra_reg
        ~scope
        ~overflow_check:true
        ~underflow_check:true
        ()
        ~capacity:(capacity_in_bytes / 2)
        ~clock:i.clocking.clock
        ~clear:i.clocking.clear
        ~wr
        ~d
        ~rd:i.bits_ready
    in
    O'.Of_signal.assign bytestream (create scope { i with bits_ready = not_full });
    not_full <== ~:(fifo.full);
    { O.jpeg_ready = bytestream.jpeg_ready
    ; done_ = bytestream.done_
    ; bits = fifo.q
    ; bits_valid = ~:(fifo.empty)
    ; markers = bytestream.markers
    ; decoder_start = bytestream.decoder_start
    }
  ;;

  let hierarchical ~capacity_in_bytes scope =
    let module Hier = Hierarchy.In_scope (I) (O) in
    Hier.hierarchical ~scope ~name:"fbytedec" (create ~capacity_in_bytes)
  ;;
end
