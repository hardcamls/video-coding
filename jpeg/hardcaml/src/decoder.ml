open! Base
open! Hardcaml
open! Signal

module I = struct
  type 'a t =
    { clocking : 'a Clocking.t
    ; start : 'a
    ; jpeg : 'a [@bits 8]
    ; jpeg_valid : 'a
    ; pixel_read_address : 'a [@bits 6]
    ; pixel_read_enable : 'a
    ; output_done : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { pixel : 'a [@bits 8]
    ; jpeg_ready : 'a
    ; start_output : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module Bytestream = Bytestream_decoder.With_fifo16
module Bitstream = Bitstream_reader
module Scan_controller = Scan_controller.With_pipeline_coontrol

let create scope (i : _ I.t) =
  let bytestream = Bytestream.O.Of_signal.wires () in
  let bitstream = Bitstream.O.Of_signal.wires () in
  let controller = Scan_controller.O.Of_signal.wires () in
  let datapath =
    Decoder_datapath.hierarchical
      scope
      { Decoder_datapath.I.clocking = i.clocking
      ; start_codeblock_decoder = controller.starts.codeblock_decoder
      ; start_idct = controller.starts.idct
      ; start_output = controller.starts.output
      ; dht = bytestream.markers.dht
      ; dqt = bytestream.markers.dqt
      ; ac_table_select = controller.ac_table_select
      ; dc_table_select = controller.dc_table_select
      ; qnt_table_select = controller.qnt_table_select
      ; dc_pred_in = controller.dc_pred_out
      ; bits = bitstream.bits
      ; bits_valid = bitstream.bits_valid
      ; pixel_read_address = i.pixel_read_address
      ; pixel_read_enable = i.pixel_read_enable
      }
  in
  Bytestream.O.Of_signal.assign
    bytestream
    (Bytestream.hierarchical
       ~capacity_in_bytes:512
       scope
       { Bytestream.I.clocking = i.clocking
       ; start = i.start
       ; jpeg = i.jpeg
       ; jpeg_valid = i.jpeg_valid
       ; bits_ready = bitstream.jpeg_ready
       ; decoder_done = datapath.done_
       });
  Bitstream.O.Of_signal.assign
    bitstream
    (Bitstream.hierarchical
       scope
       { Bitstream.I.clocking = i.clocking
       ; start = bytestream.decoder_start
       ; read_bits = datapath.read_bits
       ; jpeg_in = bytestream.bits
       ; jpeg_valid = bytestream.bits_valid
       });
  Scan_controller.O.Of_signal.assign
    controller
    (Scan_controller.hierarchical
       scope
       { Scan_controller.I.clocking = i.clocking
       ; start = bytestream.decoder_start
       ; sof = bytestream.markers.sof
       ; sos = bytestream.markers.sos
       ; dc_pred_in = datapath.dc_pred_out
       ; dc_pred_write = datapath.dc_pred_write
       ; dones =
           { codeblock_decoder = datapath.done_
           ; idct = datapath.done_
           ; output = i.output_done
           }
       });
  { O.pixel = datapath.pixel
  ; jpeg_ready = bytestream.jpeg_ready
  ; start_output = controller.starts.output
  }
;;

let hierarchical scope =
  let module Hier = Hierarchy.In_scope (I) (O) in
  Hier.hierarchical ~scope ~name:"bdec" create
;;
