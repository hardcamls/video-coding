open! Base
open Hardcaml

(* Kintex Ultrascale+ | ~220 Mhz | ~2.7K CLB LUTs  
   Spartan 7          | ~80 Mhz  | ~2.4K Slice LUTs
*)

module I = struct
  type 'a t =
    { clocking : 'a Clocking.t
    ; start : 'a
    ; dht : 'a Markers.Dht.Fields.t
    ; dqt : 'a Markers.Dqt.Fields.t
    ; luma_or_chroma : 'a
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

open Signal

let create scope (i : _ I.t) =
  let filter_stuffed_bytes = Filter_stuffed_bytes.O.Of_signal.wires () in
  let bitstream = Bitstream_reader.O.Of_signal.wires () in
  let datapath =
    Decoder_datapath.hierarchical
      scope
      { Decoder_datapath.I.clocking = i.clocking
      ; start = i.start
      ; dht = i.dht
      ; dqt = i.dqt
      ; luma_or_chroma = i.luma_or_chroma
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
       ; header_or_entropy_mode = gnd
       ; read_header_byte = gnd
       ; read_entropy_bits = datapath.read_bits
       ; jpeg_in = filter_stuffed_bytes.o_data
       ; jpeg_valid = filter_stuffed_bytes.o_valid
       });
  { O.pixel = datapath.pixel
  ; dc_pred_out = datapath.dc_pred_out
  ; jpeg_ready = filter_stuffed_bytes.i_ready
  ; done_ = datapath.done_
  }
;;
