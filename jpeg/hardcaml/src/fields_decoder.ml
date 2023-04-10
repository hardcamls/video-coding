open! Base
open Hardcaml
open Signal
module M = Fields_decoder_intf.M
module Ports = Fields_decoder_intf.Ports
module Var = Always.Variable

module Make (Fields : Interface.S) = struct
  include Ports (Fields)

  let num_bytes_to_decode =
    assert (Fields.sum_of_port_widths % 8 = 0);
    Fields.sum_of_port_widths / 8
  ;;

  let log_num_bytes_to_decode = address_bits_for num_bytes_to_decode

  module State = struct
    type t =
      | Start
      | Stream
    [@@deriving sexp_of, compare, enumerate]
  end

  let create _scope (i : _ I.t) =
    let sm = Always.State_machine.create (module State) (Clocking.to_spec i.clocking) in
    let count = Clocking.Var.reg i.clocking ~width:log_num_bytes_to_decode in
    let shift_reg = Clocking.Var.reg i.clocking ~width:Fields.sum_of_port_widths in
    let read = Clocking.Var.reg i.clocking ~width:1 in
    Always.(
      compile
        [ sm.switch
            [ Start, [ count <--. 0; when_ i.start [ read <-- vdd; sm.set_next Stream ] ]
            ; ( Stream
              , [ when_
                    i.bits_valid
                    [ count <-- count.value +:. 1
                    ; shift_reg
                      <-- shift_reg.value.:[Fields.sum_of_port_widths - 8 - 1, 0]
                          @: i.bits
                    ; when_
                        (count.value ==:. num_bytes_to_decode - 1)
                        [ read <-- gnd; sm.set_next Start ]
                    ]
                ] )
            ]
        ]);
    let fields = Fields.Of_signal.unpack ~rev:true shift_reg.value in
    { O.read_bits = read.value; fields; done_ = sm.is Start }
  ;;

  let hierarchical ?name scope =
    let module Hier = Hierarchy.In_scope (I) (O) in
    Hier.hierarchical ~scope ~name:(Option.value ~default:"flddec" name) create
  ;;
end
