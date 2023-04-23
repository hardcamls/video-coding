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
    }
  [@@deriving sexp_of, hardcaml]
end

module All_markers = struct
  type 'a t =
    { sof : 'a Markers.Sof.Fields.t [@rtlprefix "sof$"]
    ; sos : 'a Markers.Sos.Fields.t [@rtlprefix "sos$"]
    ; dqt : 'a Markers.Dqt.Fields.t [@rtlprefix "dqt$"]
    ; dht : 'a Markers.Dht.Fields.t [@rtlprefix "dht$"]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { jpeg_ready : 'a
    ; done_ : 'a
    ; bits : 'a [@bits 8]
    ; bits_valid : 'a
    ; markers : 'a All_markers.t
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
                      [ start_ecs <-- gnd; sm.set_next Entropy_coded_segment ]
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
            , [ jpeg_ready <-- vdd
              ; bits_valid <-- i.jpeg_valid
              ; when_
                  (i.jpeg_valid &: i.bits_ready &: (i.jpeg ==:. 0xff))
                  [ sm.set_next Entropy_marker ]
              ] )
          ; ( Entropy_marker
            , [ jpeg_ready <-- vdd
              ; bits_valid <-- vdd
              ; when_
                  i.jpeg_valid
                  [ sm.set_next Entropy_coded_segment
                  ; when_ (i.jpeg ==:. M.eoi &: i.bits_ready) [ sm.set_next End_of_image ]
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
  ; bits = zero 8
  ; bits_valid = bits_valid.value
  }
;;
