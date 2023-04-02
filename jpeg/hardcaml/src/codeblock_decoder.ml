(* Decoding of a codeblock *)
open Base
open Hardcaml
open Signal

module I = struct
  type 'a t =
    { clocking : 'a Clocking.t
    ; dht_header : 'a Markers.Dht.Header.Fields.t
    ; dht_code : 'a Markers.Dht.Code.t
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
        Codeword_decoder.hierarchical
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
