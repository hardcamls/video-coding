open! Base
open Hardcaml
open Signal
module Var = Always.Variable

module Sof = struct
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

module Dqt = struct
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
    Always.(
      compile
        [ sm.switch
            [ ( Start
              , [ done_ <-- vdd; when_ i.start [ done_ <-- gnd; sm.set_next Header ] ] )
            ; Header, [ when_ header.done_ [ sm.set_next Table ] ]
            ; Table, [ sm.set_next Start ]
            ]
        ]);
    Header.O.Of_signal.assign
      header
      (Header.hierarchical
         scope
         ~name:"dqthdr"
         { Header.I.clocking = i.clocking; start = i.start; bits = i.bits });
    { O.read_bits = header.read_bits
    ; fields = { fields = header.fields; element = zero 16 }
    ; done_ = done_.value
    }
  ;;

  let hierarchical ?(name = "dqt") scope =
    let module Hier = Hierarchy.In_scope (I) (O) in
    Hier.hierarchical ~scope ~name create
  ;;
end
