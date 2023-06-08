open! Base
open! Hardcaml
open! Signal

module I = struct
  type 'a t =
    { clocking : 'a Clocking.t
    ; rle_in : 'a Run_length_encode.Rle_out.t
    ; luma : 'a
    ; bits_writer_ready : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { num_bits : 'a [@bits 5]
    ; bits : 'a [@bits 16]
    }
  [@@deriving sexp_of, hardcaml]
end

module State = struct
  type t =
    | Code
    | Magnitude
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
  let size = size (module Signal) i.rle_in.value.coef in
  let mag = mag (module Signal) size i.rle_in.value.coef in
  let { Tables.Code.code = code_value; bits = code_bits } =
    Tables.create ~size ~run:i.rle_in.value.run ~luma:i.luma ~dc:i.rle_in.value.dc
  in
  Always.(
    compile
      [ sm.switch
          [ Code, [ when_ i.rle_in.valid [ sm.set_next Magnitude ] ]
          ; Magnitude, [ sm.set_next Code ]
          ]
      ]);
  { O.num_bits = mux2 (sm.is Code) (mux2 i.rle_in.valid code_bits (zero 5)) (ue size)
  ; bits = mux2 (sm.is Code) code_value (uresize mag 16)
  }
;;

let hierarchical scope =
  let module Hier = Hierarchy.In_scope (I) (O) in
  Hier.hierarchical ~scope ~name:"huff" create
;;
