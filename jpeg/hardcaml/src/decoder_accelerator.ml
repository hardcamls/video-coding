open! Base
open! Hardcaml

(* Kintex Ultrascale+ | ~220 Mhz | ~2.7K CLB LUTs  
   Spartan 7          | ~80 Mhz  | ~2.4K Slice LUTs
*)

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
    ; jpeg : 'a [@bits 16]
    ; jpeg_valid : 'a
    ; pixel_read_address : 'a [@bits 6]
    ; pixel_read_enable : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { pixel : 'a [@bits 8]
    ; dc_pred_out : 'a [@bits 12]
    ; jpeg_ready : 'a
    ; done_ : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

let create scope (i : _ I.t) =
  let filter_stuffed_bytes = Filter_stuffed_bytes.O.Of_signal.wires () in
  let bitstream = Bitstream_reader.O.Of_signal.wires () in
  let datapath =
    Decoder_datapath.hierarchical
      scope
      { Decoder_datapath.I.clocking = i.clocking
      ; start_codeblock_decoder = i.start_codeblock_decoder
      ; start_idct = i.start_idct
      ; start_output = Signal.gnd
      ; dht = i.dht
      ; dqt = i.dqt
      ; ac_table_select = i.ac_table_select
      ; dc_table_select = i.dc_table_select
      ; qnt_table_select = i.qnt_table_select
      ; dc_pred_in = i.dc_pred_in
      ; bits = bitstream.bits
      ; bits_valid = bitstream.bits_valid
      ; pixel_read_address = i.pixel_read_address
      ; pixel_read_enable = i.pixel_read_enable
      }
  in
  Filter_stuffed_bytes.O.Of_signal.assign
    filter_stuffed_bytes
    (Filter_stuffed_bytes.hierarchical
       scope
       { Filter_stuffed_bytes.I.clocking = i.clocking
       ; i_data = i.jpeg
       ; i_valid = i.jpeg_valid
       ; o_ready = bitstream.jpeg_ready
       });
  Bitstream_reader.O.Of_signal.assign
    bitstream
    (Bitstream_reader.hierarchical
       scope
       { Bitstream_reader.I.clocking = i.clocking
       ; start = i.start_codeblock_decoder
       ; read_bits = datapath.read_bits
       ; jpeg_in = filter_stuffed_bytes.o_data
       ; jpeg_valid = filter_stuffed_bytes.o_valid
       });
  { O.pixel = datapath.pixel
  ; dc_pred_out =
      Clocking.reg i.clocking ~enable:datapath.dc_pred_write datapath.dc_pred_out
  ; jpeg_ready = filter_stuffed_bytes.i_ready
  ; done_ = datapath.done_
  }
;;
