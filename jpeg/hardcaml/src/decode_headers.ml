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
      ; bits : 'a [@bits 8]
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
      { read_bits : 'a
      ; markers : 'a All_markers.t
      ; error : 'a Error.t [@rtlprefix "error_"]
      ; done_ : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module State = struct
    type t =
      | Start
      | Find_ff
      | Check_marker
      | Sof
      | Sos
      | Dqt
      | Dht
      (* | Dri *)
      | Skip_length
      | Skip
      | Error
    [@@deriving sexp_of, compare, enumerate]
  end

  let create scope (i : _ I.t) =
    let ( -- ) = Scope.naming scope in
    let sm = Always.State_machine.create (module State) (Clocking.to_spec i.clocking) in
    ignore (sm.current -- "STATE");
    let read_bits = Clocking.Var.reg i.clocking ~width:1 in
    let error = Error.Of_always.wire zero in
    let dht = Markers.Dht.O.Of_signal.wires () in
    let start_dht = Clocking.Var.reg i.clocking ~width:1 in
    let dqt = Markers.Dqt.O.Of_signal.wires () in
    let start_dqt = Clocking.Var.reg i.clocking ~width:1 in
    let sof = Markers.Sof.O.Of_signal.wires () in
    let start_sof = Clocking.Var.reg i.clocking ~width:1 in
    let sos = Markers.Sos.O.Of_signal.wires () in
    let start_sos = Clocking.Var.reg i.clocking ~width:1 in
    let skip_count = Clocking.Var.reg i.clocking ~width:16 in
    let skip_length = Clocking.Var.reg i.clocking ~width:16 in
    ignore (skip_length.value -- "SKIP_LENGTH" : Signal.t);
    let skip_length_next = skip_length.value.:[7, 0] @: i.bits in
    let skip_count_next = skip_count.value +:. 1 in
    ignore (skip_count.value -- "SKIP_COUNT" : Signal.t);
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
            [ Start, [ when_ i.start [ read_bits <-- vdd; sm.set_next Find_ff ] ]
            ; ( Find_ff
              , [ when_
                    i.bits_valid
                    [ when_ (i.bits ==:. 0xFF) [ sm.set_next Check_marker ] ]
                ] )
            ; ( Check_marker
              , [ skip_count <--. 0
                ; when_
                    i.bits_valid
                    [ sm.set_next Skip_length
                    ; switch
                        i.bits
                        ([ ( Marker_code.sof0
                           , [ read_bits <-- gnd; start_sof <-- vdd; sm.set_next Sof ] )
                         ; ( Marker_code.sos
                           , [ read_bits <-- gnd; start_sos <-- vdd; sm.set_next Sos ] )
                         ; ( Marker_code.dqt
                           , [ read_bits <-- gnd; start_dqt <-- vdd; sm.set_next Dqt ] )
                         ; ( Marker_code.dht
                           , [ read_bits <-- gnd; start_dht <-- vdd; sm.set_next Dht ] )
                         ; Marker_code.soi, [ sm.set_next Find_ff ]
                         ; Marker_code.eoi, [ sm.set_next Find_ff ]
                         ; 0xff, [ sm.set_next Check_marker ]
                         ]
                        |> List.map ~f:(fun (s, c) -> Signal.of_int ~width:8 s, c))
                    ]
                ] )
            ; Sof, [ when_ sof.done_ [ read_bits <-- vdd; sm.set_next Find_ff ] ]
            ; Sos, [ when_ sos.done_ [ sm.set_next Start ] ]
            ; Dqt, [ when_ dqt.done_ [ read_bits <-- vdd; sm.set_next Find_ff ] ]
            ; Dht, [ when_ dht.done_ [ read_bits <-- vdd; sm.set_next Find_ff ] ]
            ; ( Skip_length
              , [ when_
                    i.bits_valid
                    [ skip_count <-- skip_count_next
                    ; skip_length <-- skip_length_next
                    ; when_
                        skip_count.value.:(0)
                        [ skip_count <--. 3
                        ; if_
                            (skip_length_next ==:. 2)
                            [ sm.set_next Find_ff ]
                            [ sm.set_next Skip ]
                        ]
                    ]
                ] )
            ; ( Skip
              , [ when_
                    i.bits_valid
                    [ skip_count <-- skip_count_next
                    ; when_
                        (skip_count.value ==: skip_length.value)
                        [ sm.set_next Find_ff ]
                    ]
                ] )
            ; Error, []
            ]
        ]);
    Markers.Dht.O.Of_signal.assign
      dht
      (Markers.Dht.hierarchical
         scope
         { Markers.Dht.I.clocking = i.clocking
         ; start = start_dht.value
         ; bits = i.bits
         ; bits_valid = i.bits_valid
         });
    Markers.Dqt.O.Of_signal.assign
      dqt
      (Markers.Dqt.hierarchical
         scope
         { Markers.Dqt.I.clocking = i.clocking
         ; start = start_dqt.value
         ; bits = i.bits
         ; bits_valid = i.bits_valid
         });
    Markers.Sof.O.Of_signal.assign
      sof
      (Markers.Sof.hierarchical
         scope
         { Markers.Sof.I.clocking = i.clocking
         ; start = start_sof.value
         ; bits = i.bits
         ; bits_valid = i.bits_valid
         });
    Markers.Sos.O.Of_signal.assign
      sos
      (Markers.Sos.hierarchical
         scope
         { Markers.Sos.I.clocking = i.clocking
         ; start = start_sos.value
         ; bits = i.bits
         ; bits_valid = i.bits_valid
         });
    let read_bits =
      dht.read_bits |: dqt.read_bits |: sof.read_bits |: sos.read_bits |: read_bits.value
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
  open Signal

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
           ; advance_bits =
               mux2
                 (vld.read_bits &: reader.bits_out_available)
                 (of_int ~width:5 8)
                 (zero 5)
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
           ; bits = reader.bits.:[15, 8]
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
