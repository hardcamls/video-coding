open! Base
open! Hardcaml
open! Signal

module Coef = struct
  type 'a t =
    { coef : 'a [@bits 12]
    ; run : 'a [@bits 4]
    }
  [@@deriving sexp_of, hardcaml]
end

module I = struct
  type 'a t =
    { clocking : 'a Clocking.t
    ; start : 'a
    ; coef : 'a Coef.t
    ; luma : 'a
    ; bits_writer_ready : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { bits : 'a [@bits 5]
    ; write_bits : 'a [@bits 16]
    ; coef_address : 'a [@bits 6]
    ; coef_read_enable : 'a
    ; done_ : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module State = struct
  type t =
    | Start
    | Preroll
    | Dc
    | Dc_magnitude
    | Ac
    | Ac_magnitude
  [@@deriving sexp_of, compare, enumerate]
end

module Var = Always.Variable

(* XX Consider moving the size mag calculation upstream.  It could even be in the quantiser. *)

let size (type a) (module Comb : Comb.S with type t = a) (v : a) : a =
  let open Comb in
  let v = mux2 (msb v) (negate v) v in
  let clz = leading_zeros v in
  of_int ~width:(width clz) (width v) -: clz
;;

let mag (type a) (module Comb : Comb.S with type t = a) (size : a) (v : a) : a =
  let open Comb in
  let w = width v in
  let sign = msb v in
  let v = mux2 sign (v -:. 1) v in
  let mask = mux size (List.init w ~f:(fun i -> of_int ~width:w ((1 lsl i) - 1))) in
  v &: mask
;;

module Tables = struct
  module Code = struct
    type 'a t =
      { code : 'a [@bits 16]
      ; bits : 'a [@bits 5]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Tables = Hardcaml_jpeg_model.Tables

  let create_codes select codes =
    Array.map codes ~f:(fun { Tables.length; bits; data = _ } ->
        Code.Of_signal.of_ints { Code.code = bits; bits = length })
    |> Array.to_list
    |> Code.Of_signal.mux select
  ;;

  let dc_luma_codes = Tables.Encoder.dc_table Tables.Default.dc_luma
  let dc_luma ~size = create_codes size dc_luma_codes
  let dc_chroma_codes = Tables.Encoder.dc_table Tables.Default.dc_chroma
  let dc_chroma ~size = create_codes size dc_chroma_codes
  let ac_luma_codes = Tables.Encoder.ac_table Tables.Default.ac_luma

  let ac_luma ~size ~run =
    Array.map ac_luma_codes ~f:(fun codes -> create_codes size codes)
    |> Array.to_list
    |> Code.Of_signal.mux run
  ;;

  let ac_chroma_codes = Tables.Encoder.ac_table Tables.Default.ac_chroma

  let ac_chroma ~size ~run =
    Array.map ac_chroma_codes ~f:(fun codes -> create_codes size codes)
    |> Array.to_list
    |> Code.Of_signal.mux run
  ;;

  let create ~size ~run ~luma ~dc =
    let dc_luma = dc_luma ~size in
    let dc_chroma = dc_chroma ~size in
    let ac_luma = ac_luma ~size ~run in
    let ac_chroma = ac_chroma ~size ~run in
    let dc_sel = Code.Of_signal.mux2 luma dc_luma dc_chroma in
    let ac_sel = Code.Of_signal.mux2 luma ac_luma ac_chroma in
    Code.Of_signal.mux2 dc dc_sel ac_sel
  ;;
end

let create scope (i : _ I.t) =
  let ( -- ) = Scope.naming scope in
  let sm = Always.State_machine.create (module State) (Clocking.to_spec i.clocking) in
  ignore (sm.current -- "STATE" : Signal.t);
  let size = size (module Signal) i.coef.coef in
  let _mag = mag (module Signal) size i.coef.coef in
  let dc = Var.wire ~default:gnd in
  let { Tables.Code.code = code_value; bits = num_bits } =
    Tables.create ~size ~run:i.coef.run ~luma:i.luma ~dc:dc.value
  in
  let address = Clocking.Var.reg i.clocking ~width:6 in
  let address_next = address.value +:. 1 in
  let read_enable = Var.wire ~default:gnd in
  let ac_count = Clocking.Var.reg i.clocking ~width:6 in
  ignore (ac_count.value -- "ac_count" : Signal.t);
  Always.(
    compile
      [ sm.switch
          [ ( Start
            , [ address <--. 0; ac_count <--. 0; when_ i.start [ sm.set_next Preroll ] ] )
          ; Preroll, [ read_enable <-- vdd; address <-- address_next; sm.set_next Dc ]
          ; Dc, [ dc <-- vdd; sm.set_next Dc_magnitude ]
          ; ( Dc_magnitude
            , [ dc <-- vdd
              ; read_enable <-- vdd
              ; address <-- address_next
              ; sm.set_next Ac
              ] )
          ; ( Ac
            , [ ac_count <-- ac_count.value +: uresize i.coef.run 6 +:. 1
              ; sm.set_next Ac_magnitude
              ] )
          ; ( Ac_magnitude
            , [ read_enable <-- vdd
              ; address <-- address_next
              ; sm.set_next Ac
              ; when_ (ac_count.value ==:. 63) [ sm.set_next Start ]
              ] )
          ]
      ]);
  { O.bits = num_bits
  ; write_bits = code_value
  ; coef_address = address.value
  ; coef_read_enable = read_enable.value
  ; done_ = sm.is Start
  }
;;
