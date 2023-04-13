open! Base
open Hardcaml
open Signal
module Var = Always.Variable

module I = struct
  type 'a t =
    { clocking : 'a Clocking.t
    ; i_data : 'a [@bits 16]
    ; i_valid : 'a
    ; o_ready : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { o_data : 'a [@bits 16]
    ; o_valid : 'a
    ; i_ready : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

type byte =
  { data : Signal.t
  ; is_ff : Signal.t
  ; is_00 : Signal.t
  }

let byte ( -- ) i_data lo =
  let ( -- ) s n = s -- (n ^ Int.to_string (if lo = 0 then 2 else 1)) in
  let data = i_data.:[lo + 7, lo] in
  { data = data -- "data_"
  ; is_ff = (data ==:. 0xff) -- "is_ff_"
  ; is_00 = (data ==:. 0x00) -- "is_00_"
  }
;;

let create scope (i : _ I.t) =
  let ( -- ) = Scope.naming scope in
  let o =
    { (O.Of_always.reg (Clocking.to_spec i.clocking)) with
      i_ready = Var.wire ~default:gnd
    }
  in
  let first_byte = byte ( -- ) i.i_data 8 in
  let second_byte = byte ( -- ) i.i_data 0 in
  (* We have a byte buffered *)
  let have_buffered_byte = Clocking.Var.reg i.clocking ~width:1 in
  ignore (have_buffered_byte.value -- "have_buffered_byte" : Signal.t);
  let buffered_byte = Clocking.Var.reg i.clocking ~width:8 in
  (* the last word contained a ff, so we might need to filter a 00 *)
  let last_byte_was_ff = (buffered_byte.value ==:. 0xff) -- "last_byte_was_ff" in
  let remove_first_byte = last_byte_was_ff &: first_byte.is_00 -- "remove_first_byte" in
  let remove_second_byte =
    first_byte.is_ff &: second_byte.is_00 -- "remove_second_byte"
  in
  (* data out (when available on input) 
     {v
      have_buffered, remove_first, remove_second      o_data         buffer
           0              0              0        first @: second  
           1              0              0        buffer @: first     second
           0              1              0                            second
           1              1              0        buffer @: second
           0              0              1                            first
           1              0              1        buffer @: first
     v}
  *)
  let move_data_downstream =
    Always.(
      proc
        [ o.i_ready <-- vdd
        ; o.o_valid <-- vdd
        ; if_
            (remove_first_byte |: remove_second_byte)
            [ (* One or the other byte is being removed *)
              if_
                remove_first_byte
                [ if_
                    have_buffered_byte.value
                    [ o.o_data <-- buffered_byte.value @: second_byte.data
                    ; have_buffered_byte <-- gnd
                    ; buffered_byte <--. 0
                    ]
                    [ buffered_byte <-- second_byte.data
                    ; have_buffered_byte <-- vdd
                    ; o.o_valid <-- gnd
                    ]
                ]
                [ (* removing 2nd byte *)
                  if_
                    have_buffered_byte.value
                    [ o.o_data <-- buffered_byte.value @: first_byte.data
                    ; have_buffered_byte <-- gnd
                    ; buffered_byte <--. 0
                    ]
                    [ buffered_byte <-- first_byte.data
                    ; have_buffered_byte <-- vdd
                    ; o.o_valid <-- gnd
                    ]
                ]
            ]
            [ buffered_byte <-- second_byte.data
            ; if_
                have_buffered_byte.value
                [ o.o_data <-- buffered_byte.value @: first_byte.data ]
                [ o.o_data <-- first_byte.data @: second_byte.data ]
            ]
        ])
  in
  Always.(
    compile
      [ if_
          o.o_valid.value
          [ when_
              i.o_ready
              [ if_ i.i_valid [ move_data_downstream ] [ o.o_valid <-- gnd ] ]
          ]
          [ when_ i.i_valid [ move_data_downstream ] ]
      ]);
  O.Of_always.value o
;;
