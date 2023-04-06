open! Base
open Hardcaml

module Core = struct
  open Signal
  module Var = Always.Variable
  module Marker_code = Hardcaml_jpeg_model.Marker_code

  module I = struct
    type 'a t =
      { clocking : 'a Clocking.t
      ; start : 'a
      ; bits : 'a [@bits 16]
      ; bits_valid : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Error = struct
    module T = struct
      type t =
        | Ok
        | Decode_error
      [@@deriving sexp_of, compare, enumerate, variants]

      let to_string =
        let arr =
          Array.of_list (List.map all ~f:(fun v -> Sexp.to_string (sexp_of_t v)))
        in
        fun v -> arr.(Variants.to_rank v)
      ;;
    end

    include T
    include Enum.Make_binary (T)
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
      { read_bits : 'a [@bits 5] (* 0..16 *)
      ; markers : 'a All_markers.t
      ; error : 'a Error.t [@rtlprefix "error_"]
      ; done_ : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module State = struct
    type t =
      | Start
      | Scan_markers
      | Sof
      | Sos
      | Dqt
      | Dht
      | Dri
      | Error
    [@@deriving sexp_of, compare, enumerate]
  end

  let create scope (i : _ I.t) =
    let ( -- ) = Scope.naming scope in
    let sm = Always.State_machine.create (module State) (Clocking.to_spec i.clocking) in
    ignore (sm.current -- "STATE");
    let read_bits = Var.wire ~default:(zero 5) in
    let error = Error.Of_always.wire zero in
    let dht = Markers.Dht.O.Of_signal.wires () in
    let start_dht = Clocking.Var.reg i.clocking ~width:1 in
    let dqt = Markers.Dqt.O.Of_signal.wires () in
    let start_dqt = Clocking.Var.reg i.clocking ~width:1 in
    let sof = Markers.Sof.O.Of_signal.wires () in
    let start_sof = Clocking.Var.reg i.clocking ~width:1 in
    let sos = Markers.Sos.O.Of_signal.wires () in
    let start_sos = Clocking.Var.reg i.clocking ~width:1 in
    let _set_error e =
      Always.proc
        [ Error.Of_always.assign error (Error.of_enum (module Signal) e)
        ; sm.set_next Error
        ]
    in
    Always.(
      compile
        [ Error.Of_always.assign error (Error.of_enum (module Signal) Ok)
        ; start_dht <-- gnd
        ; start_dqt <-- gnd
        ; start_sof <-- gnd
        ; start_sos <-- gnd
        ; sm.switch
            [ Start, [ when_ i.start [ sm.set_next Scan_markers ] ]
            ; ( Scan_markers
              , [ when_
                    i.bits_valid
                    [ read_bits <--. 8
                    ; when_
                        (i.bits.:[7, 0] ==:. 0xff)
                        [ switch
                            i.bits.:[15, 8]
                            ([ ( Marker_code.sof0
                               , [ read_bits <--. 16; start_sof <-- vdd; sm.set_next Sof ]
                               )
                             ; ( Marker_code.sos
                               , [ read_bits <--. 16; start_sos <-- vdd; sm.set_next Sos ]
                               )
                             ; ( Marker_code.dqt
                               , [ read_bits <--. 16; start_dqt <-- vdd; sm.set_next Dqt ]
                               )
                             ; ( Marker_code.dht
                               , [ read_bits <--. 16; start_dht <-- vdd; sm.set_next Dht ]
                               )
                             ; Marker_code.dri, [ read_bits <--. 16 ]
                             ]
                            |> List.map ~f:(fun (s, c) -> Signal.of_int ~width:8 s, c))
                        ]
                    ]
                ] )
            ; Sof, [ when_ sof.done_ [ sm.set_next Scan_markers ] ]
            ; Sos, [ when_ sos.done_ [ sm.set_next Start ] ]
            ; Dqt, [ when_ dqt.done_ [ sm.set_next Scan_markers ] ]
            ; Dht, [ when_ dht.done_ [ sm.set_next Scan_markers ] ]
            ; Dri, []
            ; Error, []
            ]
        ]);
    Markers.Dht.O.Of_signal.assign
      dht
      (Markers.Dht.hierarchical
         scope
         { Markers.Dht.I.clocking = i.clocking; start = start_dht.value; bits = i.bits });
    Markers.Dqt.O.Of_signal.assign
      dqt
      (Markers.Dqt.hierarchical
         scope
         { Markers.Dqt.I.clocking = i.clocking; start = start_dqt.value; bits = i.bits });
    Markers.Sof.O.Of_signal.assign
      sof
      (Markers.Sof.hierarchical
         scope
         { Markers.Sof.I.clocking = i.clocking; start = start_sof.value; bits = i.bits });
    Markers.Sos.O.Of_signal.assign
      sos
      (Markers.Sos.hierarchical
         scope
         { Markers.Sos.I.clocking = i.clocking; start = start_sos.value; bits = i.bits });
    let read_bits =
      priority_select_with_default
        [ { valid = ~:(dht.done_); value = dht.read_bits }
        ; { valid = ~:(dqt.done_); value = dqt.read_bits }
        ; { valid = ~:(sof.done_); value = sof.read_bits }
        ; { valid = ~:(sos.done_); value = sos.read_bits }
        ]
        ~default:read_bits.value
    in
    { O.read_bits
    ; markers = { dht = dht.fields; dqt = dqt.fields; sof = sof.fields; sos = sos.fields }
    ; error = Error.Of_always.value error
    ; done_ = sm.is Start
    }
  ;;

  let hierarchical scope =
    let module Hier = Hierarchy.In_scope (I) (O) in
    Hier.hierarchical ~scope ~name:"vld" create
  ;;
end

module With_reader = struct
  module I = struct
    type 'a t =
      { clocking : 'a Clocking.t
      ; start : 'a
      ; bits_in : 'a [@bits 16]
      ; bits_in_available : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { markers : 'a Core.All_markers.t
      ; error : 'a Core.Error.t [@rtlprefix "error_"]
      ; read_bits_in : 'a
      ; done_ : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create scope (i : _ I.t) =
    let reader = Bitstream_reader.O.Of_signal.wires () in
    let vld = Core.O.Of_signal.wires () in
    Bitstream_reader.(
      O.Of_signal.assign
        reader
        (hierarchical
           scope
           { I.clocking = i.clocking
           ; advance_bits = vld.read_bits
           ; bits_in = i.bits_in
           ; bits_in_available = i.bits_in_available
           }));
    Core.(
      O.Of_signal.assign
        vld
        (hierarchical
           scope
           { Core.I.clocking = i.clocking
           ; start = i.start
           ; bits = reader.bits
           ; bits_valid = reader.bits_out_available
           }));
    { O.markers = vld.markers
    ; error = vld.error
    ; read_bits_in = reader.read_bits_in
    ; done_ = vld.done_
    }
  ;;

  let hierarchical scope =
    let module Hier = Hierarchy.In_scope (I) (O) in
    Hier.hierarchical ~scope ~name:"vld" create
  ;;
end
