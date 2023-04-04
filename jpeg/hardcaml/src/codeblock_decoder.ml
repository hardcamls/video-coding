(* Decoding of a codeblock *)
open Base
open Hardcaml
open Signal

module I = struct
  type 'a t =
    { clocking : 'a Clocking.t
    ; dht_header : 'a Markers.Dht.Header.Fields.t
    ; dht_code : 'a Markers.Dht.Code.t
    ; dht_code_data : 'a Markers.Dht.Code_data.t
    ; start : 'a
    ; table_id : 'a
    ; bits : 'a [@bits 16]
    }
  [@@deriving sexp_of, hardcaml]
end

module Errors = struct
  type 'a t =
    { dc_coef_decode : 'a
    ; ac_coef_decode : 'a
    ; too_many_ac_coefs : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { done_ : 'a
    ; read_bits : 'a [@bits 5]
    ; errors : 'a Errors.t
    }
  [@@deriving sexp_of, hardcaml]
end

module State = struct
  type t =
    | Start
    | Dc
    | Dc_size
    | Ac
    | Ac_size
  [@@deriving sexp_of, enumerate, compare]
end

module Decoded_code = struct
  type 'a t =
    { bits : 'a
    ; data : 'a
    ; matches : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

let codeword_decoder ~name ~acdc ~id scope (i : _ I.t) =
  let ( -- ) = Scope.naming scope in
  let ( -- ) s n = s -- (name ^ "dec$" ^ n) in
  let module T = struct
    type t =
      { data_ram_depth : int
      ; data_ram_width : int
      ; data_ram_address_bits : int
      ; class_ : int
      }
  end
  in
  let table_spec =
    match acdc with
    | `dc ->
      { T.data_ram_depth = 12; data_ram_width = 4; data_ram_address_bits = 4; class_ = 0 }
    | `ac ->
      { T.data_ram_depth = 16 * 12
      ; data_ram_width = 8
      ; data_ram_address_bits = 8
      ; class_ = 1
      }
  in
  let write =
    let dest_id = i.dht_header.destination_identifier.:(0) ==:. id in
    let class_ = i.dht_header.table_class.:(0) ==:. table_spec.class_ in
    dest_id &: class_
  in
  let code_in =
    let code = i.dht_code in
    { code with code_write = code.code_write &: write }
  in
  let code =
    Codeword_decoder.hierarchical
      scope
      ~name
      { Codeword_decoder.I.clocking = i.clocking; code_in; bits = i.bits }
  in
  let data =
    memory
      table_spec.data_ram_depth
      ~write_port:
        { write_clock = i.clocking.clock
        ; write_address =
            i.dht_code_data.data_address.:+[0, Some table_spec.data_ram_address_bits]
            -- "write_address"
        ; write_data =
            i.dht_code_data.data.:+[0, Some table_spec.data_ram_width] -- "write_data"
        ; write_enable = (i.dht_code_data.data_write &: write) -- "write_enable"
        }
      ~read_address:
        (code.decoded.data_address.:+[0, Some table_spec.data_ram_address_bits]
        -- "code_read_address")
  in
  Decoded_code.Of_signal.apply_names
    ~naming_op:( -- )
    { Decoded_code.bits = code.decoded.length; data; matches = code.matches }
;;

(* let magnitude cat value =
  let sign = mux cat (gnd :: List.init 11 ~f:(fun i -> value.:(i))) in
  let positive = 
    List.init 12
  sign
;;V *)

let create scope (i : _ I.t) =
  let ( -- ) = Scope.naming scope in
  let dc =
    List.init 2 ~f:(fun id ->
        codeword_decoder ~name:[%string "dc%{id#Int}"] ~acdc:`dc ~id scope i)
    |> Decoded_code.Of_signal.mux i.table_id
  in
  let ac =
    List.init 2 ~f:(fun id ->
        codeword_decoder ~name:[%string "ac%{id#Int}"] ~acdc:`ac ~id scope i)
    |> Decoded_code.Of_signal.mux i.table_id
  in
  let ac_run = ac.data.:[7, 4] -- "ac_run" in
  let ac_run_and_size_is_zero = (ac.data ==:. 0) -- "ac_run_and_size_is_zero" in
  let sm = Always.State_machine.create (module State) (Clocking.to_spec i.clocking) in
  ignore (sm.current -- "STATE" : Signal.t);
  let coef_count = Clocking.Var.reg i.clocking ~width:7 in
  ignore (coef_count.value -- "coef_count" : Signal.t);
  let coef_count_plus_run = coef_count.value +: uresize ac_run 7 +:. 1 in
  let errors = Errors.Of_always.reg (Clocking.to_spec i.clocking) in
  let on_error cond error_signal =
    Always.(when_ cond [ error_signal <-- vdd; sm.set_next Start ])
  in
  let size = Clocking.Var.reg i.clocking ~width:4 in
  Always.(
    compile
      [ sm.switch
          [ ( Start
            , [ when_
                  i.start
                  [ (* clear error signals *)
                    Errors.Of_always.assign errors (Errors.Of_signal.of_int 0)
                  ; sm.set_next Dc
                  ]
              ] )
          ; ( Dc
            , [ coef_count <--. 1
              ; size <-- dc.data.:[3, 0]
              ; sm.set_next Dc_size
              ; on_error ~:(dc.matches) errors.dc_coef_decode
              ] )
          ; Dc_size, [ sm.set_next Ac ]
          ; ( Ac
            , [ coef_count <-- coef_count_plus_run
              ; size <-- ac.data.:[3, 0]
              ; sm.set_next Ac_size
              ; when_
                  ac_run_and_size_is_zero
                  [ (* XX output the final run *) sm.set_next Start ]
              ; on_error ~:(ac.matches) errors.ac_coef_decode
              ] )
          ; ( Ac_size
            , [ sm.set_next Ac
              ; when_ (coef_count.value ==:. 64) [ sm.set_next Start ]
              ; on_error (coef_count.value >:. 64) errors.too_many_ac_coefs
              ] )
          ]
      ]);
  { O.done_ = sm.is Start
  ; read_bits =
      priority_select_with_default
        ~default:(zero 5)
        [ { valid = sm.is Dc; value = dc.bits }
        ; { valid = sm.is Dc_size |: sm.is Ac_size; value = uresize size.value 5 }
        ; { valid = sm.is Ac; value = ac.bits }
        ]
  ; errors = Errors.Of_always.value errors
  }
;;

let hierarchical scope =
  let module Hier = Hierarchy.In_scope (I) (O) in
  Hier.hierarchical ~scope ~name:"cblock" create
;;
