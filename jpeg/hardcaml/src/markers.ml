open! Base
open Hardcaml
open Signal
module Var = Always.Variable

module Component = struct
  module Fields = struct
    type 'a t =
      { identifier : 'a [@bits 8]
      ; vertical_sampling_factor : 'a [@bits 4]
      ; horizontal_sampling_factor : 'a [@bits 4]
      ; quantization_table_identifier : 'a [@bits 8]
      }
    [@@deriving sexp_of, hardcaml]
  end

  include Fields_decoder.Make (Fields)
end

module Sof = struct
  module Header = struct
    module Fields = struct
      type 'a t =
        { length : 'a [@bits 16]
        ; sample_precision : 'a [@bits 8]
        ; height : 'a [@bits 16]
        ; width : 'a [@bits 16]
        ; number_of_components : 'a [@bits 8]
        }
      [@@deriving sexp_of, hardcaml]
    end

    include Fields_decoder.Make (Fields)
  end

  module I = struct
    type 'a t =
      { clocking : 'a Clocking.t
      ; start : 'a
      ; bits : 'a [@bits 16]
      }
    [@@deriving sexp_of, hardcaml]
  end

  let component_address_bits = 2

  module Fields = struct
    type 'a t =
      { frame_header : 'a Header.Fields.t
      ; component : 'a Component.Fields.t
      ; component_address : 'a [@bits component_address_bits]
      ; component_write : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { read_bits : 'a [@bits 5]
      ; fields : 'a Fields.t
      ; done_ : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module State = struct
    type t =
      | Start
      | Header
      | Components
    [@@deriving sexp_of, enumerate, compare]
  end

  let create scope (i : _ I.t) =
    let sm = Always.State_machine.create (module State) (Clocking.to_spec i.clocking) in
    let done_ = Var.wire ~default:gnd in
    let header = Header.O.Of_signal.wires () in
    let component = Component.O.Of_signal.wires () in
    let start_component = Clocking.Var.reg i.clocking ~width:1 in
    let component_address = Clocking.Var.reg i.clocking ~width:component_address_bits in
    let component_address_next = component_address.value +:. 1 in
    let component_write = Var.wire ~default:gnd in
    Always.(
      compile
        [ start_component <-- gnd
        ; sm.switch
            [ ( Start
              , [ done_ <-- vdd
                ; component_address <--. 0
                ; when_ i.start [ done_ <-- gnd; sm.set_next Header ]
                ] )
            ; ( Header
              , [ when_ header.done_ [ start_component <-- vdd; sm.set_next Components ] ]
              )
            ; ( Components
              , [ when_
                    (~:(start_component.value) &: component.done_)
                    [ start_component <-- vdd
                    ; component_write <-- vdd
                    ; component_address <-- component_address_next
                    ; when_
                        Uop.(
                          component_address_next ==: header.fields.number_of_components)
                        [ start_component <-- gnd; sm.set_next Start ]
                    ]
                ] )
            ]
        ]);
    Header.O.Of_signal.assign
      header
      (Header.hierarchical
         scope
         ~name:"sofhdr"
         { Header.I.clocking = i.clocking; start = i.start; bits = i.bits });
    Component.O.Of_signal.assign
      component
      (Component.hierarchical
         scope
         ~name:"compt"
         { Component.I.clocking = i.clocking
         ; start = start_component.value
         ; bits = i.bits
         });
    { O.read_bits =
        priority_select_with_default
          ~default:header.read_bits
          [ { valid = ~:(header.done_); value = header.read_bits }
          ; { valid = ~:(component.done_); value = component.read_bits }
          ]
    ; fields =
        { frame_header = header.fields
        ; component = component.fields
        ; component_address = component_address.value
        ; component_write = component_write.value
        }
    ; done_ = done_.value
    }
  ;;

  let hierarchical ?(name = "sof") scope =
    let module Hier = Hierarchy.In_scope (I) (O) in
    Hier.hierarchical ~scope ~name create
  ;;
end

module Dqt = struct
  (* 8 bit, 64 element tables only. *)

  module Header = struct
    module Fields = struct
      type 'a t =
        { length : 'a [@bits 16]
        ; element_precision : 'a [@bits 4]
        ; table_identifier : 'a [@bits 4]
        }
      [@@deriving sexp_of, hardcaml]
    end

    include Fields_decoder.Make (Fields)
  end

  module I = struct
    type 'a t =
      { clocking : 'a Clocking.t
      ; start : 'a
      ; bits : 'a [@bits 16]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Fields = struct
    type 'a t =
      { fields : 'a Header.Fields.t
      ; element : 'a [@bits 16]
      ; element_address : 'a [@bits 6]
      ; element_write : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { read_bits : 'a [@bits 5]
      ; fields : 'a Fields.t
      ; done_ : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module State = struct
    type t =
      | Start
      | Header
      | Table
    [@@deriving sexp_of, enumerate, compare]
  end

  let create scope (i : _ I.t) =
    let sm = Always.State_machine.create (module State) (Clocking.to_spec i.clocking) in
    let done_ = Var.wire ~default:gnd in
    let header = Header.O.Of_signal.wires () in
    let count = Clocking.Var.reg i.clocking ~width:6 in
    let count_next = count.value +:. 1 in
    let element_write = Var.wire ~default:gnd in
    Always.(
      compile
        [ sm.switch
            [ ( Start
              , [ done_ <-- vdd; when_ i.start [ done_ <-- gnd; sm.set_next Header ] ] )
            ; Header, [ when_ header.done_ [ sm.set_next Table; count <--. 0 ] ]
            ; ( Table
              , [ count <-- count_next
                ; element_write <-- vdd
                ; when_ (count.value ==:. 63) [ sm.set_next Start ]
                ] )
            ]
        ]);
    Header.O.Of_signal.assign
      header
      (Header.hierarchical
         scope
         ~name:"dqthdr"
         { Header.I.clocking = i.clocking; start = i.start; bits = i.bits });
    { O.read_bits = mux2 element_write.value (of_int ~width:5 8) header.read_bits
    ; fields =
        { fields = header.fields
        ; element = zero 8 @: i.bits.:[7, 0]
        ; element_address = count.value
        ; element_write = element_write.value
        }
    ; done_ = done_.value
    }
  ;;

  let hierarchical ?(name = "dqt") scope =
    let module Hier = Hierarchy.In_scope (I) (O) in
    Hier.hierarchical ~scope ~name create
  ;;
end
