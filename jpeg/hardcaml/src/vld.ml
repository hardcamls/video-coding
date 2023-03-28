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

  module O = struct
    type 'a t =
      { coef : 'a [@bits 12]
      ; run : 'a [@bits 4]
      ; write : 'a
      ; read_bits : 'a [@bits 5] (* 0..16 *)
      ; dqt : 'a Markers.Dqt.Fields.t
      ; error : 'a Error.t [@rtlprefix "error_"]
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
    let set_error e =
      Always.proc
        [ Error.Of_always.assign error (Error.of_enum (module Signal) e)
        ; sm.set_next Error
        ]
    in
    let dqt = Markers.Dqt.O.Of_signal.wires () in
    let start_dqt = Var.wire ~default:gnd in
    Always.(
      compile
        [ read_bits <--. 0
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
                            ([ Marker_code.sof0, [ read_bits <--. 16; sm.set_next Sof ]
                             ; Marker_code.sos, [ read_bits <--. 16; sm.set_next Sos ]
                             ; ( Marker_code.dqt
                               , [ read_bits <--. 16; start_dqt <-- vdd; sm.set_next Dqt ]
                               )
                             ; Marker_code.dht, [ read_bits <--. 16; sm.set_next Dht ]
                             ; Marker_code.dri, [ read_bits <--. 16 ]
                             ]
                            |> List.map ~f:(fun (s, c) -> Signal.of_int ~width:8 s, c))
                        ]
                    ]
                ] )
            ; Sof, [ set_error Decode_error ]
            ; Sos, []
            ; Dqt, [ when_ dqt.done_ [ sm.set_next Scan_markers ] ]
            ; Dht, []
            ; Dri, []
            ; Error, []
            ]
        ]);
    Markers.Dqt.O.Of_signal.assign
      dqt
      (Markers.Dqt.hierarchical
         scope
         { Markers.Dqt.I.clocking = i.clocking; start = start_dqt.value; bits = i.bits });
    let read_bits =
      priority_select_with_default
        [ { valid = ~:(dqt.done_); value = dqt.read_bits } ]
        ~default:read_bits.value
    in
    { O.coef = zero 12
    ; run = zero 4
    ; write = gnd (* ; read_bits = Clocking.reg i.clocking read_bits *)
    ; read_bits
    ; dqt = dqt.fields
    ; error = Error.Of_always.value error
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
      { coef : 'a [@bits 12]
      ; run : 'a [@bits 4]
      ; write : 'a
      ; dqt : 'a Markers.Dqt.Fields.t
      ; error : 'a Core.Error.t [@rtlprefix "error_"]
      ; read_bits_in : 'a
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
    { O.coef = vld.coef
    ; run = vld.run
    ; write = vld.write
    ; dqt = vld.dqt
    ; error = vld.error
    ; read_bits_in = reader.read_bits_in
    }
  ;;

  let hierarchical scope =
    let module Hier = Hierarchy.In_scope (I) (O) in
    Hier.hierarchical ~scope ~name:"vld" create
  ;;
end
