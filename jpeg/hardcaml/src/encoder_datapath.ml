open Base
open Hardcaml
open Signal

module I = struct
  type 'a t =
    { clocking : 'a Clocking.t
    ; pixel : 'a [@bits 8]
    ; pixel_write_address : 'a [@bits 6]
    ; pixel_write_enable : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = { q : 'a } [@@deriving sexp_of, hardcaml]
end

module Dct = Dct.Make (Dct.Dct_config)

let dct_with_rams
    ~scope
    ~(clocking : _ Clocking.t)
    ~start_dct
    ~start_vlc
    ~pixel
    ~pixel_write_address
    ~pixel_write_enable
    ~(coef_read_port : read_port)
  =
  let ( -- ) = Scope.naming scope in
  let toggle =
    Clocking.reg_fb clocking ~enable:(start_dct |: start_vlc) ~width:1 ~f:( ~: )
    -- "toggle"
  in
  let dct = Dct.O.Of_signal.wires () in
  let iram =
    Ram.create
      ~size:128
      ~collision_mode:Write_before_read
      ~write_ports:
        [| { write_clock = clocking.clock
           ; write_address = toggle @: pixel_write_address
           ; write_data = pixel
           ; write_enable = pixel_write_enable
           }
        |]
      ~read_ports:
        [| { read_clock = clocking.clock
           ; read_address = ~:toggle @: dct.read_address
           ; read_enable = dct.coef_read
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
           ; write_address = dct.write_address
           ; write_data = dct.transpose_coef_out
           ; write_enable = dct.transpose_write
           }
        |]
      ~read_ports:
        [| { read_clock = clocking.clock
           ; read_address = dct.read_address
           ; read_enable = dct.transpose_read
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
           ; write_address = toggle @: dct.write_address
           ; write_data = dct.pixel
           ; write_enable = dct.pixel_write
           }
        |]
      ~read_ports:
        [| { coef_read_port with read_address = ~:toggle @: coef_read_port.read_address }
        |]
      ()
  in
  let dct' =
    Dct.hierarchical
      scope
      { Dct.I.clocking; start = start_dct; coef = iram.(0); transpose_coef_in = tram.(0) }
  in
  Dct.O.Of_signal.assign dct dct';
  oram.(0), dct.done_
;;

let create scope (i : _ I.t) =
  let _coef, _dct_done_ =
    dct_with_rams
      ~scope
      ~clocking:i.clocking
      ~start_dct:gnd (* XX *)
      ~start_vlc:gnd
      ~pixel:i.pixel
      ~pixel_write_address:i.pixel_write_address
      ~pixel_write_enable:i.pixel_write_enable
      ~coef_read_port:
        { read_clock = i.clocking.clock; read_address = zero 6; read_enable = gnd }
  in
  (* quant pipeline *)
  let _quant =
    Quant.hierarchical
      scope
      { Quant.I.clocking = i.clocking
      ; enable = gnd
      ; table_select = zero Quant.log_num_quant_tables
      ; dct_coef = zero Quant.dct_coef_bits
      ; dct_coef_write = gnd
      ; dct_coef_address = zero 6
      ; quant = zero Quant.quant_coef_bits
      ; quant_write = gnd
      ; quant_address = zero (6 + Quant.log_num_quant_tables)
      }
  in
  let _rle =
    Run_length_encode.hierarchical
      scope
      { Run_length_encode.I.clocking = i.clocking; start = gnd; coef = zero 12 }
  in
  let _writer =
    Bitstream_writer.hierarchical
      scope
      { Bitstream_writer.I.clocking = i.clocking; bits = zero 16; num_bits = zero 5 }
  in
  (* run-length and huffman encode *)
  O.Of_signal.of_int 0
;;
