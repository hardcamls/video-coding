open Core
open Hardcaml
open Hardcaml_jpeg
open Hardcaml_waveterm

(* To run this testbench we need to
   
  1. Pull out the headers, and program them into the hardware
  2. Find Sos, and extract the entropy coded bits
  3. Remove the markers bytes
*)

module Decoder = struct
  open Signal
  module Decoder = Codeblock_decoder
  module Reader = Util.Super_simple_bitstream_reader

  module I = struct
    type 'a t =
      { clocking : 'a Clocking.t
      ; start : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { doo : 'a } [@@deriving sexp_of, hardcaml]
  end

  let create ~bits scope (i : _ I.t) =
    let reader = Reader.O.Of_signal.wires () in
    let decoder =
      Decoder.create
        scope
        { Decoder.I.clocking = i.clocking
        ; dht_header = Hardcaml_jpeg.Markers.Dht.Header.Fields.Of_signal.of_int 0
        ; dht_code = Hardcaml_jpeg.Markers.Dht.Code.Of_signal.of_int 0
        ; start = i.start
        ; bits = reader.bits
        }
    in
    Reader.O.Of_signal.assign
      reader
      (Reader.create
         ~bits
         scope
         { Reader.I.clocking = i.clocking; read_bits = decoder.read_bits });
    { O.doo = gnd }
  ;;
end

module Sim = Cyclesim.With_interface (Decoder.I) (Decoder.O)

let test ?(waves = false) filename =
  let headers, entropy_bits = Util.headers_and_entropy_coded_segment filename in
  (* let entropy_bits = String.subo ~len:1000 entropy_bits in *)
  print_s [%message (String.length entropy_bits : int)];
  print_s [%message (headers : Hardcaml_jpeg_model.Model.Header.t)];
  let sim =
    Sim.create (Decoder.create ~bits:entropy_bits (Scope.create ~flatten_design:true ()))
  in
  let waves, sim =
    if waves
    then (
      let waves, sim = Waveform.create sim in
      Some waves, sim)
    else None, sim
  in
  let inputs = Cyclesim.inputs sim in
  inputs.clocking.clear := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clocking.clear := Bits.gnd;
  inputs.start := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.start := Bits.gnd;
  waves
;;

let%expect_test "test" =
  Option.iter
    (test ~waves:true "Mouse480.jpg")
    ~f:(Waveform.print ~display_width:100 ~display_height:40 ~wave_width:2);
  [%expect
    {|
    ("String.length entropy_bits" 6281)
    (headers
     ((frame
       (((length 17) (sample_precision 8) (width 480) (height 320)
         (number_of_components 3)
         (components
          (((identifier 1) (horizontal_sampling_factor 2)
            (vertical_sampling_factor 2) (quantization_table_identifier 0))
           ((identifier 2) (horizontal_sampling_factor 1)
            (vertical_sampling_factor 1) (quantization_table_identifier 1))
           ((identifier 3) (horizontal_sampling_factor 1)
            (vertical_sampling_factor 1) (quantization_table_identifier 1)))))))
      (quant_tables
       (((length 67) (element_precision 8) (table_identifier 1)
         (elements
          (43 45 45 60 53 60 118 65 65 118 248 165 140 165 248 248 248 248 248
           248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248
           248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248
           248 248 248 248 248 248 248 248 248 248 248)))
        ((length 67) (element_precision 8) (table_identifier 0)
         (elements
          (40 28 30 35 30 25 40 35 33 35 45 43 40 48 60 100 65 60 55 55 60 123 88
           93 73 100 145 128 153 150 143 128 140 138 160 180 230 195 160 170 218
           173 138 140 200 255 203 218 238 245 255 255 255 155 193 255 255 255
           250 255 230 253 255 248)))))
      (huffman_tables
       (((length 22) (table_class 1) (destination_identifier 1)
         (lengths (1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0))
         (values ((0) (17) (1) () () () () () () () () () () () () ())))
        ((length 22) (table_class 0) (destination_identifier 1)
         (lengths (1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0))
         (values ((0) (1) (2) () () () () () () () () () () () () ())))
        ((length 45) (table_class 1) (destination_identifier 0)
         (lengths (0 2 2 1 4 1 4 2 1 4 3 1 1 0 0 0))
         (values
          (() (0 1) (2 17) (33) (3 18 49 65) (81) (4 19 34 97) (50 113) (129)
           (5 35 66 145) (20 51 82) (161) (193) () () ())))
        ((length 26) (table_class 0) (destination_identifier 0)
         (lengths (0 3 1 1 1 1 0 0 0 0 0 0 0 0 0 0))
         (values (() (0 1 2) (3) (4) (5) (6) () () () () () () () () () ())))))
      (restart_interval ())
      (scan
       (((length 12) (number_of_image_components 3)
         (scan_components
          (((selector 1) (dc_coef_selector 0) (ac_coef_selector 0))
           ((selector 2) (dc_coef_selector 1) (ac_coef_selector 1))
           ((selector 3) (dc_coef_selector 1) (ac_coef_selector 1))))
         (start_of_predictor_selection 0) (end_of_predictor_selection 63)
         (successive_approximation_bit_high 0)
         (successive_approximation_bit_low 0))))))
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────┐
    │doo               ││                                                                              │
    │                  ││────────────                                                                  │
    │                  ││                                                                              │
    │                  ││                                                                              │
    │                  ││                                                                              │
    │                  ││                                                                              │
    │                  ││                                                                              │
    │                  ││                                                                              │
    │                  ││                                                                              │
    │                  ││                                                                              │
    │                  ││                                                                              │
    │                  ││                                                                              │
    │                  ││                                                                              │
    │                  ││                                                                              │
    │                  ││                                                                              │
    │                  ││                                                                              │
    │                  ││                                                                              │
    │                  ││                                                                              │
    │                  ││                                                                              │
    │                  ││                                                                              │
    │                  ││                                                                              │
    │                  ││                                                                              │
    │                  ││                                                                              │
    │                  ││                                                                              │
    │                  ││                                                                              │
    │                  ││                                                                              │
    │                  ││                                                                              │
    │                  ││                                                                              │
    │                  ││                                                                              │
    │                  ││                                                                              │
    │                  ││                                                                              │
    │                  ││                                                                              │
    │                  ││                                                                              │
    │                  ││                                                                              │
    │                  ││                                                                              │
    │                  ││                                                                              │
    │                  ││                                                                              │
    │                  ││                                                                              │
    └──────────────────┘└──────────────────────────────────────────────────────────────────────────────┘ |}]
;;
