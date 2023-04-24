(* XX The marker codes here can be dramatically simplified.  Their structure is 
   generally just byte orientated and we dont need to use a variable width bitstream 
   reader here.  We could just stream bytes and provide a window on top of them to 
   decode.  Ok, some some added complexity in a couple of places where we decode 
   variable length arrays - but that would be manageable.  The interface types we've
   defined here should still all work.

   The key reason for such a rewrite is to reduce the combinational load on the
   bitstream reader (which also needs to be restructured) which is currently 
   *seriously* excessive.

   But anyway, it works for now...at least until we *have* to deal with the xff 
   stuffing byte issue in the entropy coded segments where things will get hairy.  
   But I'd like to get a bit further into the decoding process first.
*)

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

  include Fields_decoder.Ports (Fields)

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
         { Header.I.clocking = i.clocking
         ; start = i.start
         ; bits = i.bits
         ; bits_valid = i.bits_valid
         });
    Component.O.Of_signal.assign
      component
      (Component.hierarchical
         scope
         ~name:"compt"
         { Component.I.clocking = i.clocking
         ; start = start_component.value
         ; bits = i.bits
         ; bits_valid = i.bits_valid
         });
    { O.read_bits = header.read_bits |: component.read_bits
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

module Scan_selector = struct
  module Fields = struct
    type 'a t =
      { selector : 'a [@bits 8]
      ; dc_coef_selector : 'a [@bits 4]
      ; ac_coef_selector : 'a [@bits 4]
      }
    [@@deriving sexp_of, hardcaml]
  end

  include Fields_decoder.Make (Fields)
end

module Sos = struct
  module Header = struct
    module Fields = struct
      type 'a t =
        { length : 'a [@bits 16]
        ; number_of_image_components : 'a [@bits 8]
        }
      [@@deriving sexp_of, hardcaml]
    end

    include Fields_decoder.Make (Fields)
  end

  module Footer = struct
    module Fields = struct
      type 'a t =
        { start_of_predictor_selection : 'a [@bits 8]
        ; end_of_predictor_selection : 'a [@bits 8]
        ; successive_approximation_bit_high : 'a [@bits 4]
        ; successive_approximation_bit_low : 'a [@bits 4]
        }
      [@@deriving sexp_of, hardcaml]
    end

    include Fields_decoder.Make (Fields)
  end

  module Fields = struct
    type 'a t =
      { header : 'a Header.Fields.t
      ; scan_selector : 'a Scan_selector.Fields.t
      ; write_scan_selector : 'a
      ; footer : 'a Footer.Fields.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  include Fields_decoder.Ports (Fields)

  module State = struct
    type t =
      | Start
      | Header
      | Scans
      | Footer
    [@@deriving sexp_of, enumerate, compare]
  end

  let create scope (i : _ I.t) =
    let sm = Always.State_machine.create (module State) (Clocking.to_spec i.clocking) in
    let header = Header.O.Of_signal.wires () in
    let scan_selector = Scan_selector.O.Of_signal.wires () in
    let footer = Footer.O.Of_signal.wires () in
    let done_ = Var.wire ~default:gnd in
    let start_scan = Clocking.Var.reg i.clocking ~width:1 in
    let start_footer = Clocking.Var.reg i.clocking ~width:1 in
    let number_of_scans = Clocking.Var.reg i.clocking ~width:8 in
    let number_of_scans_next = number_of_scans.value +:. 1 in
    let write_scan_selector = Var.wire ~default:gnd in
    Always.(
      compile
        [ start_scan <-- gnd
        ; start_footer <-- gnd
        ; sm.switch
            [ ( Start
              , [ done_ <-- vdd
                ; number_of_scans <--. 0
                ; when_ i.start [ done_ <-- gnd; sm.set_next Header ]
                ] )
            ; Header, [ when_ header.done_ [ start_scan <-- vdd; sm.set_next Scans ] ]
            ; ( Scans
              , [ when_
                    (~:(start_scan.value) &: scan_selector.done_)
                    [ number_of_scans <-- number_of_scans_next
                    ; write_scan_selector <-- vdd
                    ; if_
                        (number_of_scans_next ==: header.fields.number_of_image_components)
                        [ start_footer <-- vdd; sm.set_next Footer ]
                        [ start_scan <-- vdd ]
                    ]
                ] )
            ; ( Footer
              , [ when_ (~:(start_footer.value) &: footer.done_) [ sm.set_next Start ] ] )
            ]
        ]);
    Header.O.Of_signal.assign
      header
      (Header.hierarchical
         scope
         ~name:"soshdr"
         { Header.I.clocking = i.clocking
         ; start = i.start
         ; bits = i.bits
         ; bits_valid = i.bits_valid
         });
    Scan_selector.O.Of_signal.assign
      scan_selector
      (Scan_selector.hierarchical
         scope
         ~name:"sosscan"
         { Scan_selector.I.clocking = i.clocking
         ; start = start_scan.value
         ; bits = i.bits
         ; bits_valid = i.bits_valid
         });
    Footer.O.Of_signal.assign
      footer
      (Footer.hierarchical
         scope
         ~name:"sosftr"
         { Footer.I.clocking = i.clocking
         ; start = start_footer.value
         ; bits = i.bits
         ; bits_valid = i.bits_valid
         });
    { O.read_bits = header.read_bits |: scan_selector.read_bits |: footer.read_bits
    ; fields =
        { Fields.header = header.fields
        ; scan_selector = scan_selector.fields
        ; write_scan_selector = write_scan_selector.value
        ; footer = footer.fields
        }
    ; done_ = done_.value
    }
  ;;

  let hierarchical ?(name = "sos") scope =
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

  module Fields = struct
    type 'a t =
      { fields : 'a Header.Fields.t
      ; element : 'a [@bits 16]
      ; element_address : 'a [@bits 6]
      ; element_write : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  include Fields_decoder.Ports (Fields)

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
    let element_write = Clocking.Var.reg i.clocking ~width:1 in
    Always.(
      compile
        [ sm.switch
            [ ( Start
              , [ done_ <-- vdd; when_ i.start [ done_ <-- gnd; sm.set_next Header ] ] )
            ; ( Header
              , [ when_
                    header.done_
                    [ sm.set_next Table; element_write <-- vdd; count <--. 0 ]
                ] )
            ; ( Table
              , [ when_
                    i.bits_valid
                    [ count <-- count_next
                    ; when_
                        (count.value ==:. 63)
                        [ element_write <-- gnd; sm.set_next Start ]
                    ]
                ] )
            ]
        ]);
    Header.O.Of_signal.assign
      header
      (Header.hierarchical
         scope
         ~name:"dqthdr"
         { Header.I.clocking = i.clocking
         ; start = i.start
         ; bits = i.bits
         ; bits_valid = i.bits_valid
         });
    { O.read_bits = element_write.value |: header.read_bits
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

module Dht = struct
  module Header = struct
    module Fields = struct
      type 'a t =
        { length : 'a [@bits 16]
        ; table_class : 'a [@bits 4]
        ; destination_identifier : 'a [@bits 4]
        }
      [@@deriving sexp_of, hardcaml]
    end

    include Fields_decoder.Make (Fields)
  end

  module Code = struct
    type 'a t =
      { code_length_minus1 : 'a [@bits 4]
      ; num_codes_at_length : 'a [@bits 8]
      ; code : 'a [@bits 16]
      ; code_base_address : 'a [@bits 16]
      ; code_write : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Code_data = struct
    type 'a t =
      { data : 'a [@bits 8]
      ; data_address : 'a [@bits 16]
      ; data_write : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Fields = struct
    type 'a t =
      { header : 'a Header.Fields.t [@rtlprefix "hdr$"]
      ; code : 'a Code.t
      ; code_data : 'a Code_data.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  include Fields_decoder.Ports (Fields)

  module State = struct
    type t =
      | Start
      | Header
      | Lengths
      | Values
    [@@deriving sexp_of, enumerate, compare]
  end

  let create scope (i : _ I.t) =
    (* let ( -- ) = Scope.naming scope in *)
    let sm = Always.State_machine.create (module State) (Clocking.to_spec i.clocking) in
    let done_ = Var.wire ~default:gnd in
    let header = Header.O.Of_signal.wires () in
    let count4 = Clocking.Var.reg i.clocking ~width:4 in
    let count4_next = count4.value +:. 1 in
    let address = Clocking.Var.reg i.clocking ~width:16 in
    let address_next = address.value +:. 1 in
    let total_codes = Clocking.Var.reg i.clocking ~width:16 in
    let code = Clocking.Var.reg i.clocking ~width:16 in
    let code_write = Clocking.Var.reg i.clocking ~width:1 in
    let data_write = Clocking.Var.reg i.clocking ~width:1 in
    let num_codes_at_length = i.bits.:[7, 0] in
    Always.(
      compile
        [ sm.switch
            [ ( Start
              , [ done_ <-- vdd; when_ i.start [ done_ <-- gnd; sm.set_next Header ] ] )
            ; ( Header
              , [ when_
                    header.done_
                    [ sm.set_next Lengths
                    ; code <--. 0
                    ; total_codes <--. 0
                    ; code_write <-- vdd
                    ; count4 <--. 0
                    ]
                ] )
            ; ( Lengths
              , [ when_
                    i.bits_valid
                    [ count4 <-- count4_next
                    ; code <-- sll (code.value +: uresize num_codes_at_length 16) 1
                    ; total_codes <-- total_codes.value +: uresize num_codes_at_length 16
                    ; when_
                        (count4.value ==:. 15)
                        [ code_write <-- gnd
                        ; data_write <-- vdd
                        ; address <--. 0
                        ; count4 <--. 0
                        ; sm.set_next Values
                        ]
                    ]
                ] )
            ; ( Values
              , [ when_
                    i.bits_valid
                    [ address <-- address_next
                    ; when_
                        (address_next ==: total_codes.value)
                        [ data_write <-- gnd; sm.set_next Start ]
                    ]
                ] )
            ]
        ]);
    Header.O.Of_signal.assign
      header
      (Header.hierarchical
         scope
         ~name:"dhthdr"
         { Header.I.clocking = i.clocking
         ; start = i.start
         ; bits = i.bits
         ; bits_valid = i.bits_valid
         });
    { O.read_bits = data_write.value |: code_write.value |: header.read_bits
    ; fields =
        { header = header.fields
        ; code =
            { code_length_minus1 = count4.value
            ; num_codes_at_length
            ; code = code.value
            ; code_base_address = total_codes.value
            ; code_write = code_write.value
            }
        ; code_data =
            { data = i.bits; data_address = address.value; data_write = data_write.value }
        }
    ; done_ = done_.value
    }
  ;;

  let hierarchical ?(name = "dht") scope =
    let module Hier = Hierarchy.In_scope (I) (O) in
    Hier.hierarchical ~scope ~name create
  ;;
end

module All = struct
  type 'a t =
    { sof : 'a Sof.Fields.t [@rtlprefix "sof$"]
    ; sos : 'a Sos.Fields.t [@rtlprefix "sos$"]
    ; dqt : 'a Dqt.Fields.t [@rtlprefix "dqt$"]
    ; dht : 'a Dht.Fields.t [@rtlprefix "dht$"]
    }
  [@@deriving sexp_of, hardcaml]
end
