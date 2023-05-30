open! Base
open! Hardcaml

module I = struct
  type 'a t =
    { clocking : 'a Clocking.t
    ; bits : 'a [@bits 16]
    ; num_bits : 'a [@bits 5]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { bits_out : 'a [@bits 16]
    ; bits_valid : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module Make (Comb : Comb.S) = struct
  open Comb

  let insert_at_bottom ~buffer ~data_in ~bits =
    let buffer = log_shift sll buffer bits in
    buffer |: uresize data_in (width buffer)
  ;;
end

module Var = Always.Variable
open Signal
include Make (Signal)

let create _scope (i : _ I.t) =
  let buffer_bits = 32 in
  let buffer = Clocking.Var.reg i.clocking ~width:buffer_bits in
  ignore (buffer.value -- "BUFFER" : Signal.t);
  let count = Clocking.Var.reg i.clocking ~width:(num_bits_to_represent buffer_bits) in
  ignore (count.value -- "COUNT" : Signal.t);
  let count_next = Uop.(count.value +: i.num_bits) in
  let bits_valid = count.value >=:. 16 in
  Always.(
    compile
      [ count <-- lsbs count_next
      ; when_ bits_valid [ count <-- lsbs (count_next -:. 16) ]
      ; buffer <-- insert_at_bottom ~buffer:buffer.value ~data_in:i.bits ~bits:i.num_bits
      ]);
  { O.bits_out = (log_shift srl buffer.value count.value).:[15, 0]; bits_valid }
;;
