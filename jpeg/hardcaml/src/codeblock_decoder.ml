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
    ; bits : 'a [@bits 16]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { done_ : 'a
    ; read_bits : 'a [@bits 5]
    }
  [@@deriving sexp_of, hardcaml]
end

module State = struct
  type t =
    | Start
    | Dc
    | Ac
  [@@deriving sexp_of, enumerate, compare]
end

let codeword_decoder ~name ~acdc ~id scope (i : _ I.t) =
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
  let code_in =
    let code = i.dht_code in
    let wr =
      let dest_id = i.dht_header.destination_identifier.:(0) ==:. id in
      let class_ = i.dht_header.table_class.:(0) ==:. table_spec.class_ in
      dest_id &: class_
    in
    { code with code_write = code.code_write &: wr }
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
            i.dht_code_data.data_address.:+[table_spec.data_ram_address_bits, None]
        ; write_data = i.dht_code_data.data.:+[table_spec.data_ram_width, None]
        ; write_enable = i.dht_code_data.data_write
        }
      ~read_address:code.decoded.data_address.:+[table_spec.data_ram_address_bits, None]
  in
  data
;;

let create scope (i : _ I.t) =
  let load_code acdc id =
    let code = i.dht_code in
    let wr =
      let dest_id = i.dht_header.destination_identifier.:(0) ==:. id in
      let class_ =
        match acdc with
        | `dc -> i.dht_header.table_class.:(0) ==:. 0
        | `ac -> i.dht_header.table_class.:(0) ==:. 1
      in
      dest_id &: class_
    in
    { code with code_write = code.code_write &: wr }
  in
  let dc =
    Array.init 2 ~f:(fun id ->
        codeword_decoder
          scope
          ~name:[%string "dc%{id#Int}"]
          { Codeword_decoder.I.clocking = i.clocking
          ; code_in = load_code `dc id
          ; bits = i.bits
          })
  in
  let ac =
    Array.init 2 ~f:(fun id ->
        Codeword_decoder.hierarchical
          scope
          ~name:[%string "ac%{id#Int}"]
          { Codeword_decoder.I.clocking = i.clocking
          ; code_in = load_code `ac id
          ; bits = i.bits
          })
  in
  let sm = Always.State_machine.create (module State) (Clocking.to_spec i.clocking) in
  let count = Clocking.Var.reg i.clocking ~width:6 in
  Always.(
    compile
      [ sm.switch
          [ Start, [ when_ i.start [ sm.set_next Dc ] ]
          ; Dc, [ count <--. 1; sm.set_next Ac ]
          ; Ac, [ sm.set_next Start ]
          ]
      ]);
  { O.done_ = sm.is Start
  ; read_bits =
      priority_select_with_default
        ~default:(zero 5)
        [ { valid = sm.is Dc; value = dc.(0).decoded.length }
        ; { valid = sm.is Ac; value = ac.(0).decoded.length }
        ]
  }
;;
