open Base
open Hardcaml
open Signal
module Code = Markers.Dht.Code

module I = struct
  type 'a t =
    { clocking : 'a Clocking.t
    ; code_in : 'a Code.t
    ; bits : 'a [@bits 16]
    }
  [@@deriving sexp_of, hardcaml]
end

module Decoded_code = struct
  type 'a t =
    { data_address : 'a [@bits 16]
    ; length : 'a [@bits 5]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { matches : 'a
    ; decoded : 'a Decoded_code.t
    }
  [@@deriving sexp_of, hardcaml]
end

let decoder ~length scope (i : _ I.t) =
  let ( -- ) = Scope.naming scope in
  let ( -- ) s n = s -- [%string "d%{length#Int}$%{n}"] in
  (* Latch the code entry for this decoder *)
  let code =
    Code.Of_signal.apply_names ~naming_op:( -- ) ~prefix:"code$"
    @@ Code.map
         i.code_in
         ~f:
           (Clocking.reg
              i.clocking
              ~enable:
                (i.code_in.code_write &: (i.code_in.code_length_minus1 ==:. length - 1)))
  in
  let code_max =
    Clocking.reg
      i.clocking
      Uop.(code.code.:[length - 1, 0] +: code.num_codes_at_length -:. 1).:[length - 1, 0]
    -- "code_max"
  in
  let has_codes_at_length =
    Clocking.reg i.clocking (code.num_codes_at_length <>:. 0) -- "has_codes"
  in
  let bits = i.bits.:-[None, length] -- "bits" in
  let valid =
    let gte_min = bits >=: code.code.:[length - 1, 0] in
    let lte_max = bits <=: code_max in
    (gte_min &: lte_max &: has_codes_at_length) -- "valid"
  in
  (* XXX code base address doesnt need to be 16 bits for DC coefs *)
  (* XXX we can subtract out the base address from the code before this *)
  let offset =
    code.code_base_address +: uresize bits 16 -: uresize code.code 16 -- "offset"
  in
  let length = Signal.of_int ~width:5 length in
  { With_valid.valid; value = { Decoded_code.data_address = offset; length } }
;;

let create scope (i : _ I.t) =
  (* parallel matches *)
  let { With_valid.valid; value } =
    List.init 16 ~f:(fun length_minus1 -> decoder ~length:(length_minus1 + 1) scope i)
    |> Decoded_code.Of_signal.priority_select
  in
  { O.matches = valid; decoded = value }
;;

let hierarchical scope ~name =
  let module Hier = Hierarchy.In_scope (I) (O) in
  Hier.hierarchical ~scope ~name create
;;
