open! Base
open! Hardcaml

module I = struct
  type 'a t =
    { clocking : 'a Clocking.t
    ; start_codeblock_decoder : 'a
    ; start_idct : 'a
    ; dht : 'a Markers.Dht.Fields.t
    ; dqt : 'a Markers.Dqt.Fields.t
    ; ac_table_select : 'a [@bits Scan_controller.log_max_scans]
    ; dc_table_select : 'a [@bits Scan_controller.log_max_scans]
    ; qnt_table_select : 'a [@bits Scan_controller.log_max_components]
    ; dc_pred_in : 'a [@bits 12]
    ; bits : 'a [@bits 16]
    ; bits_valid : 'a
    ; pixel_read_address : 'a [@bits 6]
    ; pixel_read_enable : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { pixel : 'a [@bits 8]
    ; read_bits : 'a [@bits 5]
    ; dc_pred_out : 'a [@bits 12]
    ; dc_pred_write : 'a
    ; done_ : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

open Signal
module Idct = Dct.Make (Dct.Idct_config)

let idct_with_rams
    ~scope
    ~(clocking : _ Clocking.t)
    ~start
    ~(coefs : _ Codeblock_decoder.Idct_coefs.t)
    ~(output_read_port : read_port)
  =
  let ( -- ) = Scope.naming scope in
  let toggle = Clocking.reg_fb clocking ~enable:start ~width:1 ~f:( ~: ) -- "toggle" in
  let idct = Idct.O.Of_signal.wires () in
  let iram =
    Ram.create
      ~size:128
      ~collision_mode:Write_before_read
      ~write_ports:
        [| { write_clock = clocking.clock
           ; write_address = toggle @: coefs.address
           ; write_data = coefs.coef
           ; write_enable = coefs.write
           }
        |]
      ~read_ports:
        [| { read_clock = clocking.clock
           ; read_address = ~:toggle @: idct.read_address
           ; read_enable = idct.coef_read
           }
        |]
      ()
  in
  let tram =
    Ram.create
      ~size:64
      ~collision_mode:Write_before_read
      ~write_ports:
        [| { write_clock = clocking.clock
           ; write_address = idct.write_address
           ; write_data = idct.transpose_coef_out
           ; write_enable = idct.transpose_write
           }
        |]
      ~read_ports:
        [| { read_clock = clocking.clock
           ; read_address = idct.read_address
           ; read_enable = idct.transpose_read
           }
        |]
      ()
  in
  let oram =
    Ram.create
      ~size:128
      ~collision_mode:Write_before_read
      ~write_ports:
        [| { write_clock = clocking.clock
           ; write_address = toggle @: idct.write_address
           ; write_data = idct.pixel
           ; write_enable = idct.pixel_write
           }
        |]
      ~read_ports:
        [| { output_read_port with
             read_address = ~:toggle @: output_read_port.read_address
           }
        |]
      ()
  in
  let idct' =
    Idct.hierarchical
      scope
      { Idct.I.clocking; start; coef = iram.(0); transpose_coef_in = tram.(0) }
  in
  Idct.O.Of_signal.assign idct idct';
  oram.(0), idct.done_
;;

let done_delay ~count clocking ~start ~done_ =
  match count with
  | 0 -> done_
  | n ->
    let module S = struct
      type t =
        | Start
        | Done
        | Sync
      [@@deriving compare, sexp_of, enumerate]
    end
    in
    let sm = Always.State_machine.create (module S) (Clocking.to_spec clocking) in
    let count = Clocking.Var.reg clocking ~width:(num_bits_to_represent n) in
    Always.(
      compile
        [ sm.switch
            [ Start, [ when_ start [ sm.set_next Done ] ]
            ; Done, [ when_ done_ [ sm.set_next Sync ] ]
            ; ( Sync
              , [ count <-- count.value +:. 1
                ; when_ (count.value ==:. n - 1) [ count <--. 0; sm.set_next Start ]
                ] )
            ]
        ]);
    sm.is Start
;;

let create scope (i : _ I.t) =
  let codeblock_decoder =
    Codeblock_decoder.hierarchical
      scope
      { Codeblock_decoder.I.clocking = i.clocking
      ; dht = i.dht
      ; start = i.start_codeblock_decoder
      ; table_id = i.dc_table_select.:(0)
      ; bits = i.bits
      ; bits_valid = i.bits_valid
      ; dc_pred = i.dc_pred_in
      }
  in
  let dequant =
    Dequant.hierarchical
      scope
      { Dequant.I.clocking = i.clocking
      ; coefs_in = codeblock_decoder.idct_coefs
      ; dqt = i.dqt
      ; table_select = i.qnt_table_select.:(0)
      }
  in
  let pixel, idct_done =
    let output_read_port =
      ({ read_clock = i.clocking.clock
       ; read_address = i.pixel_read_address
       ; read_enable = i.pixel_read_enable
       }
        : read_port)
    in
    idct_with_rams
      ~scope
      ~clocking:i.clocking
      ~start:i.start_idct
      ~coefs:dequant.coefs_out
      ~output_read_port
  in
  (* level shift output - dct has clipped to -128 -> 127 *)
  let pixel = pixel +:. 128 in
  { O.pixel
  ; read_bits = codeblock_decoder.read_bits
  ; dc_pred_out = codeblock_decoder.idct_coefs.coef
  ; dc_pred_write = codeblock_decoder.write_dc_pred
  ; done_ =
      done_delay
        ~count:2
        i.clocking
        ~start:i.start_codeblock_decoder
        ~done_:codeblock_decoder.done_
      &: done_delay ~count:2 i.clocking ~start:i.start_idct ~done_:idct_done
  }
;;

let hierarchical scope =
  let module Hier = Hierarchy.In_scope (I) (O) in
  Hier.hierarchical ~scope ~name:"decdp" create
;;
