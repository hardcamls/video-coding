open! Base
open Hardcaml
open Signal
module M = Fields_decoder_intf.M
module Var = Always.Variable

module Make (Fields : Interface.S) = struct
  module I = struct
    type 'a t =
      { clocking : 'a Clocking.t
      ; start : 'a
      ; bits : 'a [@bits 16]
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
    type t = string [@@deriving sexp_of, compare]

    let all = Fields.(to_list port_names)
    let num_states = List.length all

    let next current =
      let rec index i = function
        | [] -> raise_s [%message "Invalid state" (current : string)]
        | h :: t ->
          if String.equal current h then (i + 1) % num_states else index (i + 1) t
      in
      let index = index 0 all in
      List.nth_exn all index
    ;;
  end

  let create _scope (i : _ I.t) =
    let sm = Always.State_machine.create (module State) (Clocking.to_spec i.clocking) in
    let read_bits = Var.wire ~default:(zero 5) in
    let fields = Fields.Of_always.reg (Clocking.to_spec i.clocking) in
    let states =
      Fields.(
        map3 port_names port_widths fields ~f:(fun name width field ->
            ( name
            , Always.
                [ read_bits <--. width
                ; (field <-- if width = 16 then bswap i.bits else i.bits.:[width - 1, 0])
                ; sm.set_next (State.next name)
                ] )))
    in
    let states =
      Fields.to_list states
      |> List.mapi ~f:(fun index (s, p) ->
             if index = 0 then s, Always.[ when_ i.start p ] else s, p)
    in
    Always.(compile [ sm.switch states ]);
    { O.read_bits = read_bits.value
    ; fields = Fields.Of_always.value fields
    ; done_ = sm.is (List.hd_exn State.all) &: ~:(i.start)
    }
  ;;

  let hierarchical ?name scope =
    let module Hier = Hierarchy.In_scope (I) (O) in
    Hier.hierarchical ~scope ~name:(Option.value ~default:"flddec" name) create
  ;;
end
