(* This is the interesting marker code. 
   
  Decoding the bits is relatively straight forward, but then they need to be 
  decoded into huffman codewords.
  
  I think many hardware decoders just use the pre-defined code tables, but I 
  am kinda interested in seeing how we might support the more general case.
*)

open Core
open Hardcaml
open Hardcaml_waveterm
open Hardcaml_jpeg
open Util
module Dht = Wrapped_marker_decoder (Markers.Dht.Fields) (Markers.Dht)

let test ?(verbose = false) ~start_cycle nth_marker =
  let bits = Test_vld.load_jpeg () in
  let dht_bits =
    find_nth_marker_exn
      ~n:nth_marker
      ~marker_code:Hardcaml_jpeg_model.Marker_code.dht
      bits
  in
  if verbose then print_s [%message (dht_bits : String.Hexdump.t)];
  let on_cycle (outputs : _ Dht.O.t) =
    if Bits.to_bool !(outputs.fields.code.code_write)
    then (
      let code =
        Hardcaml_jpeg.Markers.Dht.Code.map outputs.fields.code ~f:(fun d ->
            Bits.to_int !d)
      in
      if code.num_codes_at_length <> 0
      then print_s [%message (code : int Hardcaml_jpeg.Markers.Dht.Code.t)]);
    if Bits.to_bool !(outputs.fields.code_data.data_write)
    then (
      let data =
        Hardcaml_jpeg.Markers.Dht.Code_data.map outputs.fields.code_data ~f:(fun d ->
            Bits.to_int !d)
      in
      print_s [%message (data : int Hardcaml_jpeg.Markers.Dht.Code_data.t)])
  in
  let waves = Dht.test ~waves:true ~on_cycle dht_bits in
  Option.iter
    waves
    ~f:(Waveform.print ~start_cycle ~wave_width:2 ~display_width:128 ~display_height:50)
;;

let%expect_test "1st marker" =
  test ~verbose:true ~start_cycle:4 0;
  [%expect
    {|
    (dht_bits
     ("00000000  00 1a 00 00 03 01 01 01  01 00 00 00 00 00 00 00  |................|"
      "00000010  00 00 00 00 01 02 03 04  05 06                    |..........|"))
    (code
     ((code_length_minus1 0) (num_codes_at_length 3) (code 0)
      (code_base_address 0) (code_write 1)))
    (code
     ((code_length_minus1 1) (num_codes_at_length 1) (code 6)
      (code_base_address 3) (code_write 1)))
    (code
     ((code_length_minus1 2) (num_codes_at_length 1) (code 14)
      (code_base_address 4) (code_write 1)))
    (code
     ((code_length_minus1 3) (num_codes_at_length 1) (code 30)
      (code_base_address 5) (code_write 1)))
    (code
     ((code_length_minus1 4) (num_codes_at_length 1) (code 62)
      (code_base_address 6) (code_write 1)))
    (data ((data 1) (data_address 0) (data_write 1)))
    (data ((data 2) (data_address 1) (data_write 1)))
    (data ((data 3) (data_address 2) (data_write 1)))
    (data ((data 4) (data_address 3) (data_write 1)))
    (data ((data 5) (data_address 4) (data_write 1)))
    (data ((data 6) (data_address 5) (data_write 1)))
    (data ((data 0) (data_address 6) (data_write 1)))
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────────────────────────────────┐
    │clock             ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐│
    │                  ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └│
    │clear             ││                                                                                                          │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │start             ││                                                                                                          │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││────────────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬───│
    │code              ││ 0000       │0006 │000E │001E │003E │007E │00FC │01F8 │03F0 │07E0 │0FC0 │1F80 │3F00 │7E00 │FC00 │F800 │F00│
    │                  ││────────────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴───│
    │                  ││────────────┬─────┬─────┬─────┬─────┬─────────────────────────────────────────────────────────────────────│
    │code_base_address ││ 0000       │0003 │0004 │0005 │0006 │0007                                                                 │
    │                  ││────────────┴─────┴─────┴─────┴─────┴─────────────────────────────────────────────────────────────────────│
    │                  ││────────────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬───│
    │code_length_minus1││ 0          │1    │2    │3    │4    │5    │6    │7    │8    │9    │A    │B    │C    │D    │E    │F    │0  │
    │                  ││────────────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴───│
    │code_write        ││      ┌───────────────────────────────────────────────────────────────────────────────────────────────┐   │
    │                  ││──────┘                                                                                               └───│
    │                  ││────────────┬───────────────────────┬─────────────────────────────────────────────────────────────────┬───│
    │data              ││ 03         │01                     │00                                                               │01 │
    │                  ││────────────┴───────────────────────┴─────────────────────────────────────────────────────────────────┴───│
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │data_address      ││ 0000                                                                                                     │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │data_write        ││                                                                                                      ┌───│
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────────┘   │
    │done_             ││                                                                                                          │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │hdr$destination_id││ 0                                                                                                        │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │hdr$length        ││ 1A00                                                                                                     │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │hdr$table_class   ││ 0                                                                                                        │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││────────────┬───────────────────────┬─────────────────────────────────────────────────────────────────┬───│
    │num_codes_at_lengt││ 03         │01                     │00                                                               │01 │
    │                  ││────────────┴───────────────────────┴─────────────────────────────────────────────────────────────────┴───│
    │                  ││────────────┬─────┬─────────────────┬─────┬───────────────────────────────────────────────────────────┬───│
    │BITS              ││ 0003       │0301 │0101             │0100 │0000                                                       │000│
    │                  ││────────────┴─────┴─────────────────┴─────┴───────────────────────────────────────────────────────────┴───│
    │                  ││──────┬───────────────────────────────────────────────────────────────────────────────────────────────────│
    │READ_BITS         ││ 00   │08                                                                                                 │
    │                  ││──────┴───────────────────────────────────────────────────────────────────────────────────────────────────│
    │gnd               ││                                                                                                          │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │vdd               ││──────────────────────────────────────────────────────────────────────────────────────────────────────────│
    └──────────────────┘└──────────────────────────────────────────────────────────────────────────────────────────────────────────┘ |}]
;;

let%expect_test "2nd marker" =
  test ~verbose:true ~start_cycle:4 1;
  [%expect
    {|
    (dht_bits
     ("00000000  00 2d 10 00 02 02 01 04  01 04 02 01 04 03 01 01  |.-..............|"
      "00000010  00 00 00 00 01 02 11 21  03 12 31 41 51 04 13 22  |.......!..1AQ..\"|"
      "00000020  61 32 71 81 05 23 42 91  14 33 52 a1 c1           |a2q..#B..3R..|"))
    (code
     ((code_length_minus1 0) (num_codes_at_length 2) (code 0)
      (code_base_address 0) (code_write 1)))
    (code
     ((code_length_minus1 1) (num_codes_at_length 2) (code 4)
      (code_base_address 2) (code_write 1)))
    (code
     ((code_length_minus1 2) (num_codes_at_length 1) (code 12)
      (code_base_address 4) (code_write 1)))
    (code
     ((code_length_minus1 3) (num_codes_at_length 4) (code 26)
      (code_base_address 5) (code_write 1)))
    (code
     ((code_length_minus1 4) (num_codes_at_length 1) (code 60)
      (code_base_address 9) (code_write 1)))
    (code
     ((code_length_minus1 5) (num_codes_at_length 4) (code 122)
      (code_base_address 10) (code_write 1)))
    (code
     ((code_length_minus1 6) (num_codes_at_length 2) (code 252)
      (code_base_address 14) (code_write 1)))
    (code
     ((code_length_minus1 7) (num_codes_at_length 1) (code 508)
      (code_base_address 16) (code_write 1)))
    (code
     ((code_length_minus1 8) (num_codes_at_length 4) (code 1018)
      (code_base_address 17) (code_write 1)))
    (code
     ((code_length_minus1 9) (num_codes_at_length 3) (code 2044)
      (code_base_address 21) (code_write 1)))
    (code
     ((code_length_minus1 10) (num_codes_at_length 1) (code 4094)
      (code_base_address 24) (code_write 1)))
    (code
     ((code_length_minus1 11) (num_codes_at_length 1) (code 8190)
      (code_base_address 25) (code_write 1)))
    (data ((data 1) (data_address 0) (data_write 1)))
    (data ((data 2) (data_address 1) (data_write 1)))
    (data ((data 17) (data_address 2) (data_write 1)))
    (data ((data 33) (data_address 3) (data_write 1)))
    (data ((data 3) (data_address 4) (data_write 1)))
    (data ((data 18) (data_address 5) (data_write 1)))
    (data ((data 49) (data_address 6) (data_write 1)))
    (data ((data 65) (data_address 7) (data_write 1)))
    (data ((data 81) (data_address 8) (data_write 1)))
    (data ((data 4) (data_address 9) (data_write 1)))
    (data ((data 19) (data_address 10) (data_write 1)))
    (data ((data 34) (data_address 11) (data_write 1)))
    (data ((data 97) (data_address 12) (data_write 1)))
    (data ((data 50) (data_address 13) (data_write 1)))
    (data ((data 113) (data_address 14) (data_write 1)))
    (data ((data 129) (data_address 15) (data_write 1)))
    (data ((data 5) (data_address 16) (data_write 1)))
    (data ((data 35) (data_address 17) (data_write 1)))
    (data ((data 66) (data_address 18) (data_write 1)))
    (data ((data 145) (data_address 19) (data_write 1)))
    (data ((data 20) (data_address 20) (data_write 1)))
    (data ((data 51) (data_address 21) (data_write 1)))
    (data ((data 82) (data_address 22) (data_write 1)))
    (data ((data 161) (data_address 23) (data_write 1)))
    (data ((data 193) (data_address 24) (data_write 1)))
    (data ((data 0) (data_address 25) (data_write 1)))
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────────────────────────────────┐
    │clock             ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐│
    │                  ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └│
    │clear             ││                                                                                                          │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │start             ││                                                                                                          │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││────────────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬───│
    │code              ││ 0000       │0004 │000C │001A │003C │007A │00FC │01FC │03FA │07FC │0FFE │1FFE │3FFE │7FFC │FFF8 │FFF0 │FFE│
    │                  ││────────────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴───│
    │                  ││────────────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬───────────────────────────│
    │code_base_address ││ 0000       │0002 │0004 │0005 │0009 │000A │000E │0010 │0011 │0015 │0018 │0019 │001A                       │
    │                  ││────────────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴───────────────────────────│
    │                  ││────────────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬───│
    │code_length_minus1││ 0          │1    │2    │3    │4    │5    │6    │7    │8    │9    │A    │B    │C    │D    │E    │F    │0  │
    │                  ││────────────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴───│
    │code_write        ││      ┌───────────────────────────────────────────────────────────────────────────────────────────────┐   │
    │                  ││──────┘                                                                                               └───│
    │                  ││──────────────────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬───────────┬───────────────────────┬───│
    │data              ││ 02               │01   │04   │01   │04   │02   │01   │04   │03   │01         │00                     │01 │
    │                  ││──────────────────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴───────────┴───────────────────────┴───│
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │data_address      ││ 0000                                                                                                     │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │data_write        ││                                                                                                      ┌───│
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────────┘   │
    │done_             ││                                                                                                          │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │hdr$destination_id││ 0                                                                                                        │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │hdr$length        ││ 2D00                                                                                                     │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │hdr$table_class   ││ 0                                                                                                        │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││──────────────────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬───────────┬───────────────────────┬───│
    │num_codes_at_lengt││ 02               │01   │04   │01   │04   │02   │01   │04   │03   │01         │00                     │01 │
    │                  ││──────────────────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴───────────┴───────────────────────┴───│
    │                  ││────────────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────────────────┬───│
    │BITS              ││ 0002       │0202 │0201 │0104 │0401 │0104 │0402 │0201 │0104 │0403 │0301 │0101 │0100 │0000             │000│
    │                  ││────────────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────────────────┴───│
    │                  ││──────┬───────────────────────────────────────────────────────────────────────────────────────────────────│
    │READ_BITS         ││ 00   │08                                                                                                 │
    │                  ││──────┴───────────────────────────────────────────────────────────────────────────────────────────────────│
    │gnd               ││                                                                                                          │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │vdd               ││──────────────────────────────────────────────────────────────────────────────────────────────────────────│
    └──────────────────┘└──────────────────────────────────────────────────────────────────────────────────────────────────────────┘ |}]
;;
