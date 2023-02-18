open! Base
open Hardcaml

module I = struct
  type 'a t =
    { clocking : 'a Clocking.t
    ; start : 'a
    ; bits : 'a [@bits 16]
    }
  [@@deriving sexp_of, hardcaml]
end

module Error = struct
  module T = struct
    type t =
      | Ok
      | Decode_error
    [@@deriving sexp_of, compare, enumerate, variants]

    let to_string =
      let arr = Array.of_list (List.map all ~f:(fun v -> Sexp.to_string (sexp_of_t v))) in
      fun v -> arr.(Variants.to_rank v)
    ;;
  end

  include T
  include Enum.Make_binary (T)
end

module O = struct
  type 'a t =
    { coef : 'a [@bits 12]
    ; run : 'a [@bits 4]
    ; write : 'a
    ; read_bits : 'a [@bits 5] (* 0..16 *)
    ; error : 'a Error.t
    }
  [@@deriving sexp_of, hardcaml]
end

module State = struct
  type t =
    | Start
    | Scan_markers
    | Sof
    | Sos
    | Dqt
    | Dht
    | Dri
    | Error
  [@@deriving sexp_of, compare, enumerate]
end

open Signal
module Var = Always.Variable
module Marker_code = Hardcaml_jpeg_model.Marker_code

module Fields_decoder (Fields : Interface.S) = struct
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

  let _create _scope (i : _ I.t) =
    let sm = Always.State_machine.create (module State) (Clocking.to_spec i.clocking) in
    let read_bits = Var.wire ~default:(zero 5) in
    let fields = Fields.Of_always.reg (Clocking.to_spec i.clocking) in
    let states =
      Fields.(
        map3 port_names port_widths fields ~f:(fun name width field ->
            ( name
            , Always.
                [ read_bits <--. width
                ; field <-- i.bits.:[width - 1, 0]
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
    ; done_ = sm.is (List.hd_exn State.all)
    }
  ;;
end

module Start_of_frame = struct
  module Fields = struct
    type 'a t =
      { length : 'a [@bits 16]
      ; sample_precision : 'a [@bits 8]
      ; height : 'a [@bits 16]
      ; width : 'a [@bits 16]
      ; number_of_components : 'a [@bits 8]
      }
    [@@deriving sexp_of, hardcaml]
  end

  include Fields_decoder (Fields)
end

module Component = struct
  module Fields = struct
    type 'a t =
      { identifier : 'a [@bits 8]
      ; vertical_sampling_factor : 'a [@bits 4]
      ; horizontal_sampling_factor : 'a [@bits 4]
      ; quantization_table_identifier : 'a [@bits 8]
      }
    [@@deriving sexp_of, hardcaml]
  end

  include Fields_decoder (Fields)
end

let create _scope (i : _ I.t) =
  let sm = Always.State_machine.create (module State) (Clocking.to_spec i.clocking) in
  let read_bits = Var.wire ~default:(zero 5) in
  let error = Error.Of_always.wire zero in
  let _set_error =
    Always.proc
      [ Error.Of_always.assign error (Error.of_enum (module Signal) Decode_error)
      ; sm.set_next Error
      ]
  in
  Always.(
    compile
      [ read_bits <--. 0
      ; sm.switch
          [ Start, [ when_ i.start [ sm.set_next Scan_markers ] ]
          ; ( Scan_markers
            , [ read_bits <--. 8
              ; when_
                  (i.bits.:[15, 8] ==:. 0xff)
                  [ switch
                      i.bits.:[7, 0]
                      ([ Marker_code.sof0, [ read_bits <--. 16; sm.set_next Sof ]
                       ; Marker_code.sos, [ read_bits <--. 16 ]
                       ; Marker_code.dqt, [ read_bits <--. 16 ]
                       ; Marker_code.dht, [ read_bits <--. 16 ]
                       ; Marker_code.dri, [ read_bits <--. 16 ]
                       ]
                      |> List.map ~f:(fun (s, c) -> Signal.of_int ~width:8 s, c))
                  ]
              ] )
          ; Sof, []
          ; Sos, []
          ; Dqt, []
          ; Dht, []
          ; Dri, []
          ]
      ]);
  { O.coef = zero 12
  ; run = zero 4
  ; write = gnd
  ; read_bits = read_bits.value
  ; error = Error.Of_always.value error
  }
;;
