open! Base
open! Hardcaml
open! Signal

module Core = struct
  module I = struct
    type 'a t =
      { clocking : 'a Clocking.t
      ; start : 'a
      ; markers : 'a Decode_headers.All_markers.t
      ; bits : 'a [@bits 16]
      ; bits_valid : 'a
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

  let create scope (i : _ I.t) =
    let controller = Scan_controller.O.Of_signal.wires () in
    let datapath =
      Decoder_datapath.hierarchical
        scope
        { Decoder_datapath.I.clocking = i.clocking
        ; start = controller.start.codeblock_decoder
        ; dht = i.markers.dht
        ; dqt = i.markers.dqt
        ; luma_or_chroma = controller.luma_or_chroma
        ; dc_pred_in = controller.dc_pred_out
        ; bits = i.bits
        ; bits_valid = i.bits_valid
        ; pixel_read_address = i.pixel_read_address
        ; pixel_read_enable = i.pixel_read_enable
        }
    in
    Scan_controller.O.Of_signal.assign
      controller
      (Scan_controller.hierarchical
         scope
         { Scan_controller.I.clocking = i.clocking
         ; start = i.start
         ; done_ =
             { codeblock_decoder = datapath.done_
             ; idct = datapath.done_
             ; output = i.output_done
             }
         ; dc_pred_in = datapath.dc_pred_out
         ; dc_pred_write = datapath.dc_pred_write
         });
    { O.pixel = datapath.pixel
    ; read_bits = datapath.read_bits
    ; start_output = controller.start.output
    }
  ;;

  let hierarchical scope =
    let module Hier = Hierarchy.In_scope (I) (O) in
    Hier.hierarchical ~scope ~name:"bdec" create
  ;;
end
