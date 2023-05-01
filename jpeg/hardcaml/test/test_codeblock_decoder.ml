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
  open! Signal
  module Decoder = Codeblock_decoder
  module Reader = Util.Super_simple_bitstream_reader

  module I = struct
    type 'a t =
      { clocking : 'a Clocking.t
      ; start : 'a
      ; dht : 'a Hardcaml_jpeg.Markers.Dht.Fields.t
      ; dqt : 'a Hardcaml_jpeg.Markers.Dqt.Fields.t
      ; table_id : 'a
      }
    [@@deriving sexp_of, hardcaml ~rtlmangle:"$"]
  end

  module O = struct
    type 'a t = { decoder : 'a Decoder.O.t [@rtlprefix "idct$"] }
    [@@deriving sexp_of, hardcaml]
  end

  let create ~bits scope (i : _ I.t) =
    let reader = Reader.O.Of_signal.wires () in
    let decoder =
      Decoder.hierarchical
        scope
        { Decoder.I.clocking = i.clocking
        ; dht = i.dht
        ; start = i.start
        ; table_id = i.table_id
        ; bits = reader.bits
        ; bits_valid = vdd
        ; dc_pred = zero 12
        }
    in
    let dequant =
      Dequant.hierarchical
        scope
        { Dequant.I.clocking = i.clocking
        ; coefs_in = decoder.idct_coefs
        ; dqt = i.dqt
        ; table_select = i.table_id
        }
    in
    Reader.O.Of_signal.assign
      reader
      (Reader.create
         ~bits
         scope
         { Reader.I.clocking = i.clocking; read_bits = decoder.read_bits });
    { O.decoder = { decoder with idct_coefs = dequant.coefs_out } }
  ;;
end

module Model = Hardcaml_jpeg_model.Decoder
module Sim = Cyclesim.With_interface (Decoder.I) (Decoder.O)

let test ?(waves = false) filename =
  let headers, entropy_bits = Util.headers_and_entropy_coded_segment filename in
  (* let entropy_bits = String.subo ~len:1000 entropy_bits in *)
  print_s [%message (String.subo entropy_bits ~len:64 : String.Hexdump.t)];
  print_s [%message (headers : Model.Header.t)];
  let sim =
    Sim.create
      ~config:Cyclesim.Config.trace_all
      (Decoder.create
         ~bits:entropy_bits
         (Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true ()))
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
  (* load the code words from the header *)
  let huffman_tables = Model.Header.huffman_tables headers in
  Util.load_huffman_tables ~cycle:(fun () -> Cyclesim.cycle sim) inputs.dht huffman_tables;
  let quant_tables = Model.Header.quant_tables headers in
  Util.load_quant_tables ~cycle:(fun () -> Cyclesim.cycle sim) inputs.dqt quant_tables;
  inputs.start := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.start := Bits.gnd;
  for _ = 0 to 100 do
    Cyclesim.cycle sim
  done;
  waves
;;

let%expect_test "test" =
  Option.iter
    (test ~waves:true "Mouse480.jpg")
    ~f:
      (Waveform.print
         ~display_width:100
         ~display_height:60
         ~wave_width:1
         ~start_cycle:100);
  [%expect
    {|
    ("String.subo entropy_bits ~len:64"
     ("00000000  f5 1c 53 0f 6e 3e 0c 56  f8 3e 6d 0e 1a ce e9 ac  |..S.n>.V.>m.....|"
      "00000010  04 6b ed c7 c0 6c 8f 80  53 4f b2 94 93 28 36 a0  |.k...l..SO...(6.|"
      "00000020  da 86 00 4e d4 1b 51 40  41 3b 50 6d 45 01 46 6e  |...N..Q@A;PmE.Fn|"
      "00000030  0b c0 9e 94 7c 1a 00 19  7b 51 f0 0f 4a 3e 0d 44  |....|...{Q..J>.D|"))
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
         (lengths (1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0)) (values (0 17 1)))
        ((length 22) (table_class 0) (destination_identifier 1)
         (lengths (1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0)) (values (0 1 2)))
        ((length 45) (table_class 1) (destination_identifier 0)
         (lengths (0 2 2 1 4 1 4 2 1 4 3 1 1 0 0 0))
         (values
          (0 1 2 17 33 3 18 49 65 81 4 19 34 97 50 113 129 5 35 66 145 20 51 82
           161 193)))
        ((length 26) (table_class 0) (destination_identifier 0)
         (lengths (0 3 1 1 1 1 0 0 0 0 0 0 0 0 0 0)) (values (0 1 2 3 4 5 6)))))
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
    │clocking$clear    ││                                                                              │
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │clocking$clock    ││                                                                              │
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │dht$code          ││ FC00                                                                         │
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │dht$code_base_addr││ 0007                                                                         │
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │dht$code_length_mi││ F                                                                            │
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │dht$code_write    ││                                                                              │
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │                  ││────┬───┬───┬─────────────────────────────────────────────────────────────────│
    │dht$data          ││ 03 │04 │05 │06                                                               │
    │                  ││────┴───┴───┴─────────────────────────────────────────────────────────────────│
    │                  ││────┬───┬───┬─────────────────────────────────────────────────────────────────│
    │dht$data_address  ││ 00.│00.│00.│0006                                                             │
    │                  ││────┴───┴───┴─────────────────────────────────────────────────────────────────│
    │dht$data_write    ││────────────────┐                                                             │
    │                  ││                └─────────────────────────────────────────────────────────────│
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │dht$hdr$destinatio││ 0                                                                            │
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │dht$hdr$table_clas││ 0                                                                            │
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │dht$num_codes_at_l││ 00                                                                           │
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │                  ││────────────────┬───┬───────┬───┬───┬───┬───┬───────┬───┬───┬───┬───┬───┬─────│
    │dqt$element       ││ 0000           │00.│002D   │00.│00.│00.│00.│0041   │00.│00.│00.│00.│00.│00F8 │
    │                  ││────────────────┴───┴───────┴───┴───┴───┴───┴───────┴───┴───┴───┴───┴───┴─────│
    │                  ││────────────────────┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬─│
    │dqt$element_addres││ 00                 │01 │02 │03 │04 │05 │06 │07 │08 │09 │0A │0B │0C │0D │0E │0│
    │                  ││────────────────────┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴─│
    │dqt$element_write ││                ┌─────────────────────────────────────────────────────────────│
    │                  ││────────────────┘                                                             │
    │                  ││────────────────┬─────────────────────────────────────────────────────────────│
    │dqt$table_identifi││ 0              │1                                                            │
    │                  ││────────────────┴─────────────────────────────────────────────────────────────│
    │start             ││                                                                              │
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │table_id          ││                                                                              │
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │idct$ac_coef_decod││                                                                              │
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │idct$address      ││ 00                                                                           │
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │idct$coef         ││ 000                                                                          │
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │idct$dc_coef_decod││                                                                              │
    │                  ││──────────────────────────────────────────────────────────────────────────────│
    │idct$done_        ││──────────────────────────────────────────────────────────────────────────────│
    └──────────────────┘└──────────────────────────────────────────────────────────────────────────────┘ |}]
;;
