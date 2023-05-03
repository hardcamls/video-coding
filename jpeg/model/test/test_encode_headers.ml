open! Core

include struct
  open Hardcaml_jpeg_model
  module Encoder = Encoder
  module Quant_tables = Quant_tables
  module Tables = Tables
  module Writer = Bitstream_writer
  module Reader = Bitstream_reader
  module Decoder = Decoder
end

let%expect_test "example header" =
  let quality = 20 in
  let writer = Writer.create () in
  let qnt_luma = Quant_tables.scale Quant_tables.luma quality in
  let qnt_chroma = Quant_tables.scale Quant_tables.chroma quality in
  Encoder.write_headers
    writer
    ~width:480
    ~height:320
    ~dc_luma:Tables.Default.dc_luma
    ~ac_luma:Tables.Default.ac_luma
    ~dc_chroma:Tables.Default.dc_chroma
    ~ac_chroma:Tables.Default.ac_chroma
    ~qnt_luma
    ~qnt_chroma;
  let buffer = Writer.get_buffer writer in
  print_s [%message (buffer : String.Hexdump.t)];
  let header = Decoder.Header.decode (Reader.From_string.create buffer) in
  print_s [%message (header : Decoder.Header.t)];
  [%expect
    {|
    (buffer
     ("00000000  ff d8 ff e0 00 10 48 61  72 64 63 61 6d 6c 20 4a  |......Hardcaml J|"
      "00000010  50 45 47 2e ff db 00 43  00 28 1c 19 28 3c 64 80  |PEG....C.(..(<d.|"
      "00000020  99 1e 1e 23 30 41 91 96  8a 23 21 28 3c 64 8f ad  |...#0A...#!(<d..|"
      "00000030  8c 23 2b 37 49 80 da c8  9b 2d 37 5d 8c aa ff ff  |.#+7I....-7]....|"
      "00000040  c1 3c 58 8a a0 cb ff ff  e6 7b a0 c3 da ff ff ff  |.<X......{......|"
      "00000050  fd b4 e6 ee f5 ff fa ff  f8 ff db 00 43 01 2b 2d  |............C.+-|"
      "00000060  3c 76 f8 f8 f8 f8 2d 35  41 a5 f8 f8 f8 f8 3c 41  |<v....-5A.....<A|"
      "00000070  8c f8 f8 f8 f8 f8 76 a5  f8 f8 f8 f8 f8 f8 f8 f8  |......v.........|"
      "00000080  f8 f8 f8 f8 f8 f8 f8 f8  f8 f8 f8 f8 f8 f8 f8 f8  |................|"
      "00000090  f8 f8 f8 f8 f8 f8 f8 f8  f8 f8 f8 f8 f8 f8 ff c0  |................|"
      "000000a0  00 11 08 01 40 01 e0 03  01 22 00 02 11 01 03 11  |....@....\"......|"
      "000000b0  01 ff c4 00 1f 00 00 01  05 01 01 01 01 01 01 00  |................|"
      "000000c0  00 00 00 00 00 00 00 01  02 03 04 05 06 07 08 09  |................|"
      "000000d0  0a 0b ff c4 00 1f 01 00  03 01 01 01 01 01 01 01  |................|"
      "000000e0  01 01 00 00 00 00 00 00  01 02 03 04 05 06 07 08  |................|"
      "000000f0  09 0a 0b ff c4 00 b5 10  00 02 01 03 03 02 04 03  |................|"
      "00000100  05 05 04 04 00 00 01 7d  01 02 03 00 04 11 05 12  |.......}........|"
      "00000110  21 31 41 06 13 51 61 07  22 71 14 32 81 91 a1 08  |!1A..Qa.\"q.2....|"
      "00000120  23 42 b1 c1 15 52 d1 f0  24 33 62 72 82 09 0a 16  |#B...R..$3br....|"
      "00000130  17 18 19 1a 25 26 27 28  29 2a 34 35 36 37 38 39  |....%&'()*456789|"
      "00000140  3a 43 44 45 46 47 48 49  4a 53 54 55 56 57 58 59  |:CDEFGHIJSTUVWXY|"
      "00000150  5a 63 64 65 66 67 68 69  6a 73 74 75 76 77 78 79  |Zcdefghijstuvwxy|"
      "00000160  7a 83 84 85 86 87 88 89  8a 92 93 94 95 96 97 98  |z...............|"
      "00000170  99 9a a2 a3 a4 a5 a6 a7  a8 a9 aa b2 b3 b4 b5 b6  |................|"
      "00000180  b7 b8 b9 ba c2 c3 c4 c5  c6 c7 c8 c9 ca d2 d3 d4  |................|"
      "00000190  d5 d6 d7 d8 d9 da e1 e2  e3 e4 e5 e6 e7 e8 e9 ea  |................|"
      "000001a0  f1 f2 f3 f4 f5 f6 f7 f8  f9 fa ff c4 00 b5 11 00  |................|"
      "000001b0  02 01 02 04 04 03 04 07  05 04 04 00 01 02 77 00  |..............w.|"
      "000001c0  01 02 03 11 04 05 21 31  06 12 41 51 07 61 71 13  |......!1..AQ.aq.|"
      "000001d0  22 32 81 08 14 42 91 a1  b1 c1 09 23 33 52 f0 15  |\"2...B.....#3R..|"
      "000001e0  62 72 d1 0a 16 24 34 e1  25 f1 17 18 19 1a 26 27  |br...$4.%.....&'|"
      "000001f0  28 29 2a 35 36 37 38 39  3a 43 44 45 46 47 48 49  |()*56789:CDEFGHI|"
      "00000200  4a 53 54 55 56 57 58 59  5a 63 64 65 66 67 68 69  |JSTUVWXYZcdefghi|"
      "00000210  6a 73 74 75 76 77 78 79  7a 82 83 84 85 86 87 88  |jstuvwxyz.......|"
      "00000220  89 8a 92 93 94 95 96 97  98 99 9a a2 a3 a4 a5 a6  |................|"
      "00000230  a7 a8 a9 aa b2 b3 b4 b5  b6 b7 b8 b9 ba c2 c3 c4  |................|"
      "00000240  c5 c6 c7 c8 c9 ca d2 d3  d4 d5 d6 d7 d8 d9 da e2  |................|"
      "00000250  e3 e4 e5 e6 e7 e8 e9 ea  f2 f3 f4 f5 f6 f7 f8 f9  |................|"
      "00000260  fa ff da 00 0c 03 01 00  02 11 03 11 00 3f 00     |.............?.|"))
    (header
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
          (43 45 60 118 248 248 248 248 45 53 65 165 248 248 248 248 60 65 140
           248 248 248 248 248 118 165 248 248 248 248 248 248 248 248 248 248
           248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248
           248 248 248 248 248 248 248 248 248 248 248)))
        ((length 67) (element_precision 8) (table_identifier 0)
         (elements
          (40 28 25 40 60 100 128 153 30 30 35 48 65 145 150 138 35 33 40 60 100
           143 173 140 35 43 55 73 128 218 200 155 45 55 93 140 170 255 255 193
           60 88 138 160 203 255 255 230 123 160 195 218 255 255 255 253 180 230
           238 245 255 250 255 248)))))
      (huffman_tables
       (((length 181) (table_class 1) (destination_identifier 1)
         (lengths (0 2 1 2 4 4 3 4 7 5 4 4 0 1 2 119))
         (values
          (0 1 2 3 17 4 5 33 49 6 18 65 81 7 97 113 19 34 50 129 8 20 66 145 161
           177 193 9 35 51 82 240 21 98 114 209 10 22 36 52 225 37 241 23 24 25
           26 38 39 40 41 42 53 54 55 56 57 58 67 68 69 70 71 72 73 74 83 84 85
           86 87 88 89 90 99 100 101 102 103 104 105 106 115 116 117 118 119 120
           121 122 130 131 132 133 134 135 136 137 138 146 147 148 149 150 151
           152 153 154 162 163 164 165 166 167 168 169 170 178 179 180 181 182
           183 184 185 186 194 195 196 197 198 199 200 201 202 210 211 212 213
           214 215 216 217 218 226 227 228 229 230 231 232 233 234 242 243 244
           245 246 247 248 249 250)))
        ((length 181) (table_class 1) (destination_identifier 0)
         (lengths (0 2 1 3 3 2 4 3 5 5 4 4 0 0 1 125))
         (values
          (1 2 3 0 4 17 5 18 33 49 65 6 19 81 97 7 34 113 20 50 129 145 161 8 35
           66 177 193 21 82 209 240 36 51 98 114 130 9 10 22 23 24 25 26 37 38 39
           40 41 42 52 53 54 55 56 57 58 67 68 69 70 71 72 73 74 83 84 85 86 87
           88 89 90 99 100 101 102 103 104 105 106 115 116 117 118 119 120 121
           122 131 132 133 134 135 136 137 138 146 147 148 149 150 151 152 153
           154 162 163 164 165 166 167 168 169 170 178 179 180 181 182 183 184
           185 186 194 195 196 197 198 199 200 201 202 210 211 212 213 214 215
           216 217 218 225 226 227 228 229 230 231 232 233 234 241 242 243 244
           245 246 247 248 249 250)))
        ((length 31) (table_class 0) (destination_identifier 1)
         (lengths (0 3 1 1 1 1 1 1 1 1 1 0 0 0 0 0))
         (values (0 1 2 3 4 5 6 7 8 9 10 11)))
        ((length 31) (table_class 0) (destination_identifier 0)
         (lengths (0 1 5 1 1 1 1 1 1 0 0 0 0 0 0 0))
         (values (0 1 2 3 4 5 6 7 8 9 10 11)))))
      (restart_interval ())
      (scan
       (((length 12) (number_of_image_components 3)
         (scan_components
          (((selector 1) (dc_coef_selector 0) (ac_coef_selector 0))
           ((selector 2) (dc_coef_selector 1) (ac_coef_selector 1))
           ((selector 3) (dc_coef_selector 1) (ac_coef_selector 1))))
         (start_of_predictor_selection 0) (end_of_predictor_selection 63)
         (successive_approximation_bit_high 0)
         (successive_approximation_bit_low 0)))))) |}]
;;
