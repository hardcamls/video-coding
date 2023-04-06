open! Base
open! Hardcaml

module Core = struct
  module I = struct
    type 'a t =
      { clocking : 'a Clocking.t
      ; start : 'a
      ; markers : 'a Vld.Core.All_markers.t
      ; bits : 'a [@bits 16]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { pixel : 'a [@bits 8]
      ; read_bits : 'a [@bits 5]
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
    let toggle = Clocking.reg_fb clocking ~enable:start ~width:1 ~f:( ~: ) in
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

  module Controller = struct
    module Ctrl = struct
      type 'a t =
        { codeblock_decoder : 'a
        ; idct : 'a
        ; output : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    module I = struct
      type 'a t =
        { clocking : 'a Clocking.t
        ; start : 'a
        ; done_ : 'a Ctrl.t [@rtlsuffix "_done"]
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O = struct
      type 'a t =
        { start : 'a Ctrl.t [@rtlsuffix "_start"]
        ; done_ : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    module State = struct
      type t =
        | Start
        | Y
        | Cb
        | Cr
      [@@deriving sexp_of, compare, enumerate]
    end

    module Var = Always.Variable

    (* XXX cheat to start with and assume a simple yuv420 encoding in the reasonable 
   scan order.  We'll generalize later.*)

    let create _scope (i : _ I.t) =
      let sm = Always.State_machine.create (module State) (Clocking.to_spec i.clocking) in
      let starts = Ctrl.Of_always.wire zero in
      let all_done = reduce ~f:( &: ) (Ctrl.to_list i.done_) in
      let component = Clocking.Var.reg i.clocking ~width:2 in
      let x_blk = Clocking.Var.reg i.clocking ~width:1 in
      let y_blk = Clocking.Var.reg i.clocking ~width:1 in
      let x = Clocking.Var.reg i.clocking ~width:16 in
      let y = Clocking.Var.reg i.clocking ~width:16 in
      let start_pipe = Clocking.Var.reg i.clocking ~width:2 in
      Always.(
        compile
          [ sm.switch
              [ ( Start
                , [ component <--. 0
                  ; x <--. 0
                  ; y <--. 0
                  ; x_blk <--. 0
                  ; y_blk <--. 0
                  ; when_
                      i.start
                      [ start_pipe <--. 0
                      ; starts.codeblock_decoder <-- vdd
                      ; sm.set_next Y
                      ]
                  ] )
              ; ( Y
                , [ when_
                      all_done
                      [ (* XX this assumes the start pipe flushes during the initial luma phase, 
                         which might not be true for 444 images.  I think we can do something smarter 
                         in the Ctrl module as the operation is ia pretty simple shift register thing *)
                        start_pipe <-- lsbs start_pipe.value @: vdd
                      ; starts.codeblock_decoder <-- vdd
                      ; starts.idct <-- start_pipe.value.:(0)
                      ; starts.output <-- start_pipe.value.:(1)
                      ; x_blk <-- x_blk.value +:. 1
                      ; when_
                          (x_blk.value ==:. 1)
                          [ x_blk <--. 0
                          ; y_blk <-- y_blk.value +:. 1
                          ; when_ (y_blk.value ==:. 1) [ sm.set_next Cb ]
                          ]
                      ]
                  ] )
              ; ( Cb
                , [ when_
                      all_done
                      [ starts.codeblock_decoder <-- vdd
                      ; starts.idct <-- vdd
                      ; starts.output <-- vdd
                      ; sm.set_next Cr
                      ]
                  ] )
              ; ( Cr
                , [ when_
                      all_done
                      [ starts.codeblock_decoder <-- vdd
                      ; starts.idct <-- vdd
                      ; starts.output <-- vdd
                      ; sm.set_next Cr
                      ; x <-- x.value +:. 1
                      ; when_
                          (x.value ==:. 3)
                          [ x <--. 0
                          ; y <-- y.value +:. 1
                          ; when_
                              (y.value ==:. 3)
                              [ starts.codeblock_decoder <-- gnd
                              ; starts.idct <-- gnd
                              ; starts.output <-- gnd
                              ; y <--. 0
                              ; sm.set_next Start
                              ]
                          ]
                      ]
                  ] )
              ]
          ]);
      { O.start = Ctrl.Of_always.value starts; done_ = sm.is Start }
    ;;

    let hierarchical scope =
      let module Hier = Hierarchy.In_scope (I) (O) in
      Hier.hierarchical ~scope ~name:"ctrl" create
    ;;
  end

  let create scope (i : _ I.t) =
    let table_id = gnd in
    let done_ = Controller.Ctrl.Of_signal.wires () in
    let controller =
      Controller.hierarchical
        scope
        { Controller.I.clocking = i.clocking; start = i.start; done_ }
    in
    let codeblock_decoder =
      Codeblock_decoder.hierarchical
        scope
        { Codeblock_decoder.I.clocking = i.clocking
        ; dht_header = i.markers.dht.header
        ; dht_code = i.markers.dht.code
        ; dht_code_data = i.markers.dht.code_data
        ; start = controller.start.codeblock_decoder
        ; table_id
        ; bits = i.bits
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
        ({ read_clock = i.clocking.clock; read_address = zero 6; read_enable = gnd }
          : read_port)
      in
      idct_with_rams
        ~scope
        ~clocking:i.clocking
        ~start:controller.start.idct
        ~coefs:dequant.coefs_out
        ~output_read_port
    in
    Controller.Ctrl.Of_signal.assign
      done_
      { codeblock_decoder = codeblock_decoder.done_; idct = idct_done; output = vdd };
    { O.pixel; read_bits = codeblock_decoder.read_bits }
  ;;

  let hierarchical scope =
    let module Hier = Hierarchy.In_scope (I) (O) in
    Hier.hierarchical ~scope ~name:"bdec" create
  ;;
end
