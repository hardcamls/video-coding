open! Base
open! Hardcaml

module Core = struct
  module I = struct
    type 'a t =
      { clocking : 'a Clocking.t
      ; start : 'a
      ; markers : 'a Vld.Core.All_markers.t
      ; bits : 'a [@bits 16]
      ; pixel_read_address : 'a [@bits 6]
      ; pixel_read_enable : 'a
      ; output_done : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { pixel : 'a [@bits 8]
      ; read_bits : 'a [@bits 5]
      ; start_output : 'a
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

  let done_delay ~count clocking start done_ =
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
    let table_id = gnd in
    let controller = Scan_controller.O.Of_signal.wires () in
    let codeblock_decoder =
      Codeblock_decoder.hierarchical
        scope
        { Codeblock_decoder.I.clocking = i.clocking
        ; dht = i.markers.dht
        ; start = controller.start.codeblock_decoder
        ; table_id = controller.luma_or_chroma
        ; bits = i.bits
        ; dc_pred = controller.dc_pred_out
        }
    in
    let dequant =
      Dequant.hierarchical
        scope
        { Dequant.I.clocking = i.clocking
        ; coefs_in = codeblock_decoder.idct_coefs
        ; dqt = i.markers.dqt
        ; table_select = table_id
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
        ~start:controller.start.idct
        ~coefs:dequant.coefs_out
        ~output_read_port
    in
    (* level shift output - dct has clipped to -128 -> 127 *)
    let pixel = pixel +:. 128 in
    Scan_controller.O.Of_signal.assign
      controller
      (Scan_controller.hierarchical
         scope
         { Scan_controller.I.clocking = i.clocking
         ; start = i.start
         ; done_ =
             { codeblock_decoder =
                 done_delay
                   ~count:2
                   i.clocking
                   controller.start.codeblock_decoder
                   codeblock_decoder.done_
             ; idct = done_delay ~count:2 i.clocking controller.start.idct idct_done
             ; output = i.output_done
             }
         ; dc_pred_in = codeblock_decoder.idct_coefs.coef
         ; dc_pred_write = codeblock_decoder.write_dc_pred
         });
    { O.pixel
    ; read_bits = codeblock_decoder.read_bits
    ; start_output = controller.start.output
    }
  ;;

  let hierarchical scope =
    let module Hier = Hierarchy.In_scope (I) (O) in
    Hier.hierarchical ~scope ~name:"bdec" create
  ;;
end
