open Base
open Hardcaml
open Signal

module Starts = struct
  type 'a t =
    { dct : 'a
    ; vlc : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module Pixels = struct
  type 'a t =
    { data : 'a [@bits 8]
    ; write_address : 'a [@bits 6]
    ; write_enable : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module I = struct
  type 'a t =
    { clocking : 'a Clocking.t
    ; starts : 'a Starts.t [@rtlprefix "starts$"]
    ; pixels : 'a Pixels.t [@rtlprefix "pixels$"]
    ; quant_write : 'a Quant.Quant_write.t [@rtlprefix "qnt$"]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { rle_out : 'a Run_length_encode.Rle_out.t [@rtlprefix "rle$"]
    ; bitstream : 'a Bitstream_writer.O.t [@rtlprefix "bits$"]
    ; done_ : 'a
    }
  [@@deriving sexp_of, hardcaml]
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
           ; write_data =
               pixel -:. 128
               (* level shifted input *)
               (* XXX register? *)
           ; write_enable = pixel_write_enable
           }
        |]
      ~read_ports:
        [| { read_clock = clocking.clock
           ; read_address = ~:toggle @: dct.read_address
           ; read_enable = dct.coef_in_read
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
           ; write_data = dct.coef_out
           ; write_enable = dct.coef_out_write
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
      { Dct.I.clocking
      ; start = start_dct
      ; coef_in = iram.(0)
      ; transpose_coef_in = tram.(0)
      }
  in
  Dct.O.Of_signal.assign dct dct';
  oram.(0), dct.done_
;;

let create scope (i : _ I.t) =
  let rle = Run_length_encode.O.Of_signal.wires () in
  let dct_coef, dct_done =
    dct_with_rams
      ~scope
      ~clocking:i.clocking
      ~start_dct:i.starts.dct
      ~start_vlc:i.starts.vlc
      ~pixel:i.pixels.data
      ~pixel_write_address:i.pixels.write_address
      ~pixel_write_enable:i.pixels.write_enable
      ~coef_read_port:
        { read_clock = i.clocking.clock
        ; read_address = rle.quant_address
        ; read_enable = rle.quant_read
        }
  in
  (* quant pipeline *)
  let quant =
    Quant.hierarchical
      scope
      { Quant.I.clocking = i.clocking
      ; enable = vdd
      ; table_select = zero Quant.log_num_quant_tables
      ; dct_coef
      ; dct_coef_read = rle.quant_read
      ; dct_coef_address = rle.quant_address
      ; quant = i.quant_write
      }
  in
  Run_length_encode.O.Of_signal.assign
    rle
    (Run_length_encode.hierarchical
       scope
       { Run_length_encode.I.clocking = i.clocking
       ; start = i.starts.vlc
       ; coef = quant.quant_coef
       });
  let huffman =
    Huffman_encode.hierarchical
      scope
      { Huffman_encode.I.clocking = i.clocking
      ; rle_in = rle.rle_out
      ; luma = vdd
      ; bits_writer_ready = vdd
      }
  in
  let bitstream =
    Bitstream_writer.hierarchical
      scope
      { Bitstream_writer.I.clocking = i.clocking
      ; bits = huffman.bits
      ; num_bits = huffman.num_bits
      }
  in
  (* run-length and huffman encode *)
  { O.rle_out = rle.rle_out; bitstream; done_ = dct_done &: rle.done_ }
;;

let hierarchical scope =
  let module Hier = Hierarchy.In_scope (I) (O) in
  Hier.hierarchical ~scope ~name:"encdp" create
;;
