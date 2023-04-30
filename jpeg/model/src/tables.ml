open Base

type 'a coef =
  { length : int
  ; bits : int
  ; data : 'a
  }
[@@deriving sexp_of]

type dc = int [@@deriving sexp_of]
type dc_coef = dc coef [@@deriving sexp_of]

let sort x =
  List.sort
    x
    ~compare:(fun
               { length = length1; bits = bits1; data = _ }
               { length = length2; bits = bits2; data = _ }
             -> [%compare: int * int] (length1, bits1) (length2, bits2))
;;

let dc_luma =
  [ { length = 2; bits = 0b00; data = 0 }
  ; { length = 3; bits = 0b010; data = 1 }
  ; { length = 3; bits = 0b011; data = 2 }
  ; { length = 3; bits = 0b100; data = 3 }
  ; { length = 3; bits = 0b101; data = 4 }
  ; { length = 3; bits = 0b110; data = 5 }
  ; { length = 4; bits = 0b1110; data = 6 }
  ; { length = 5; bits = 0b11110; data = 7 }
  ; { length = 6; bits = 0b111110; data = 8 }
  ; { length = 7; bits = 0b1111110; data = 9 }
  ; { length = 8; bits = 0b11111110; data = 10 }
  ; { length = 9; bits = 0b111111110; data = 11 }
  ]
  |> sort
;;

let dc_chroma =
  [ { length = 2; bits = 0b00; data = 0 }
  ; { length = 2; bits = 0b01; data = 1 }
  ; { length = 2; bits = 0b10; data = 2 }
  ; { length = 3; bits = 0b110; data = 3 }
  ; { length = 4; bits = 0b1110; data = 4 }
  ; { length = 5; bits = 0b11110; data = 5 }
  ; { length = 6; bits = 0b111110; data = 6 }
  ; { length = 7; bits = 0b1111110; data = 7 }
  ; { length = 8; bits = 0b11111110; data = 8 }
  ; { length = 9; bits = 0b111111110; data = 9 }
  ; { length = 10; bits = 0b1111111110; data = 10 }
  ; { length = 11; bits = 0b11111111110; data = 11 }
  ]
  |> sort
;;

type ac =
  { run : int
  ; size : int
  }
[@@deriving sexp_of]

type ac_coef = ac coef [@@deriving sexp_of]

let ac_luma =
  [ { length = 4; bits = 0b1010; data = { run = 0x0; size = 0x0 } }
  ; { length = 2; bits = 0b00; data = { run = 0x0; size = 0x1 } }
  ; { length = 2; bits = 0b01; data = { run = 0x0; size = 0x2 } }
  ; { length = 3; bits = 0b100; data = { run = 0x0; size = 0x3 } }
  ; { length = 4; bits = 0b1011; data = { run = 0x0; size = 0x4 } }
  ; { length = 5; bits = 0b11010; data = { run = 0x0; size = 0x5 } }
  ; { length = 7; bits = 0b1111000; data = { run = 0x0; size = 0x6 } }
  ; { length = 8; bits = 0b11111000; data = { run = 0x0; size = 0x7 } }
  ; { length = 10; bits = 0b1111110110; data = { run = 0x0; size = 0x8 } }
  ; { length = 16; bits = 0b1111111110000010; data = { run = 0x0; size = 0x9 } }
  ; { length = 16; bits = 0b1111111110000011; data = { run = 0x0; size = 0xA } }
  ; { length = 4; bits = 0b1100; data = { run = 0x1; size = 0x1 } }
  ; { length = 5; bits = 0b11011; data = { run = 0x1; size = 0x2 } }
  ; { length = 7; bits = 0b1111001; data = { run = 0x1; size = 0x3 } }
  ; { length = 9; bits = 0b111110110; data = { run = 0x1; size = 0x4 } }
  ; { length = 11; bits = 0b11111110110; data = { run = 0x1; size = 0x5 } }
  ; { length = 16; bits = 0b1111111110000100; data = { run = 0x1; size = 0x6 } }
  ; { length = 16; bits = 0b1111111110000101; data = { run = 0x1; size = 0x7 } }
  ; { length = 16; bits = 0b1111111110000110; data = { run = 0x1; size = 0x8 } }
  ; { length = 16; bits = 0b1111111110000111; data = { run = 0x1; size = 0x9 } }
  ; { length = 16; bits = 0b1111111110001000; data = { run = 0x1; size = 0xA } }
  ; { length = 5; bits = 0b11100; data = { run = 0x2; size = 0x1 } }
  ; { length = 8; bits = 0b11111001; data = { run = 0x2; size = 0x2 } }
  ; { length = 10; bits = 0b1111110111; data = { run = 0x2; size = 0x3 } }
  ; { length = 12; bits = 0b111111110100; data = { run = 0x2; size = 0x4 } }
  ; { length = 16; bits = 0b1111111110001001; data = { run = 0x2; size = 0x5 } }
  ; { length = 16; bits = 0b1111111110001010; data = { run = 0x2; size = 0x6 } }
  ; { length = 16; bits = 0b1111111110001011; data = { run = 0x2; size = 0x7 } }
  ; { length = 16; bits = 0b1111111110001100; data = { run = 0x2; size = 0x8 } }
  ; { length = 16; bits = 0b1111111110001101; data = { run = 0x2; size = 0x9 } }
  ; { length = 16; bits = 0b1111111110001110; data = { run = 0x2; size = 0xA } }
  ; { length = 6; bits = 0b111010; data = { run = 0x3; size = 0x1 } }
  ; { length = 9; bits = 0b111110111; data = { run = 0x3; size = 0x2 } }
  ; { length = 12; bits = 0b111111110101; data = { run = 0x3; size = 0x3 } }
  ; { length = 16; bits = 0b1111111110001111; data = { run = 0x3; size = 0x4 } }
  ; { length = 16; bits = 0b1111111110010000; data = { run = 0x3; size = 0x5 } }
  ; { length = 16; bits = 0b1111111110010001; data = { run = 0x3; size = 0x6 } }
  ; { length = 16; bits = 0b1111111110010010; data = { run = 0x3; size = 0x7 } }
  ; { length = 16; bits = 0b1111111110010011; data = { run = 0x3; size = 0x8 } }
  ; { length = 16; bits = 0b1111111110010100; data = { run = 0x3; size = 0x9 } }
  ; { length = 16; bits = 0b1111111110010101; data = { run = 0x3; size = 0xA } }
  ; { length = 6; bits = 0b111011; data = { run = 0x4; size = 0x1 } }
  ; { length = 10; bits = 0b1111111000; data = { run = 0x4; size = 0x2 } }
  ; { length = 16; bits = 0b1111111110010110; data = { run = 0x4; size = 0x3 } }
  ; { length = 16; bits = 0b1111111110010111; data = { run = 0x4; size = 0x4 } }
  ; { length = 16; bits = 0b1111111110011000; data = { run = 0x4; size = 0x5 } }
  ; { length = 16; bits = 0b1111111110011001; data = { run = 0x4; size = 0x6 } }
  ; { length = 16; bits = 0b1111111110011010; data = { run = 0x4; size = 0x7 } }
  ; { length = 16; bits = 0b1111111110011011; data = { run = 0x4; size = 0x8 } }
  ; { length = 16; bits = 0b1111111110011100; data = { run = 0x4; size = 0x9 } }
  ; { length = 16; bits = 0b1111111110011101; data = { run = 0x4; size = 0xA } }
  ; { length = 7; bits = 0b1111010; data = { run = 0x5; size = 0x1 } }
  ; { length = 11; bits = 0b11111110111; data = { run = 0x5; size = 0x2 } }
  ; { length = 16; bits = 0b1111111110011110; data = { run = 0x5; size = 0x3 } }
  ; { length = 16; bits = 0b1111111110011111; data = { run = 0x5; size = 0x4 } }
  ; { length = 16; bits = 0b1111111110100000; data = { run = 0x5; size = 0x5 } }
  ; { length = 16; bits = 0b1111111110100001; data = { run = 0x5; size = 0x6 } }
  ; { length = 16; bits = 0b1111111110100010; data = { run = 0x5; size = 0x7 } }
  ; { length = 16; bits = 0b1111111110100011; data = { run = 0x5; size = 0x8 } }
  ; { length = 16; bits = 0b1111111110100100; data = { run = 0x5; size = 0x9 } }
  ; { length = 16; bits = 0b1111111110100101; data = { run = 0x5; size = 0xA } }
  ; { length = 7; bits = 0b1111011; data = { run = 0x6; size = 0x1 } }
  ; { length = 12; bits = 0b111111110110; data = { run = 0x6; size = 0x2 } }
  ; { length = 16; bits = 0b1111111110100110; data = { run = 0x6; size = 0x3 } }
  ; { length = 16; bits = 0b1111111110100111; data = { run = 0x6; size = 0x4 } }
  ; { length = 16; bits = 0b1111111110101000; data = { run = 0x6; size = 0x5 } }
  ; { length = 16; bits = 0b1111111110101001; data = { run = 0x6; size = 0x6 } }
  ; { length = 16; bits = 0b1111111110101010; data = { run = 0x6; size = 0x7 } }
  ; { length = 16; bits = 0b1111111110101011; data = { run = 0x6; size = 0x8 } }
  ; { length = 16; bits = 0b1111111110101100; data = { run = 0x6; size = 0x9 } }
  ; { length = 16; bits = 0b1111111110101101; data = { run = 0x6; size = 0xA } }
  ; { length = 8; bits = 0b11111010; data = { run = 0x7; size = 0x1 } }
  ; { length = 12; bits = 0b111111110111; data = { run = 0x7; size = 0x2 } }
  ; { length = 16; bits = 0b1111111110101110; data = { run = 0x7; size = 0x3 } }
  ; { length = 16; bits = 0b1111111110101111; data = { run = 0x7; size = 0x4 } }
  ; { length = 16; bits = 0b1111111110110000; data = { run = 0x7; size = 0x5 } }
  ; { length = 16; bits = 0b1111111110110001; data = { run = 0x7; size = 0x6 } }
  ; { length = 16; bits = 0b1111111110110010; data = { run = 0x7; size = 0x7 } }
  ; { length = 16; bits = 0b1111111110110011; data = { run = 0x7; size = 0x8 } }
  ; { length = 16; bits = 0b1111111110110100; data = { run = 0x7; size = 0x9 } }
  ; { length = 16; bits = 0b1111111110110101; data = { run = 0x7; size = 0xA } }
  ; { length = 9; bits = 0b111111000; data = { run = 0x8; size = 0x1 } }
  ; { length = 15; bits = 0b111111111000000; data = { run = 0x8; size = 0x2 } }
  ; { length = 16; bits = 0b1111111110110110; data = { run = 0x8; size = 0x3 } }
  ; { length = 16; bits = 0b1111111110110111; data = { run = 0x8; size = 0x4 } }
  ; { length = 16; bits = 0b1111111110111000; data = { run = 0x8; size = 0x5 } }
  ; { length = 16; bits = 0b1111111110111001; data = { run = 0x8; size = 0x6 } }
  ; { length = 16; bits = 0b1111111110111010; data = { run = 0x8; size = 0x7 } }
  ; { length = 16; bits = 0b1111111110111011; data = { run = 0x8; size = 0x8 } }
  ; { length = 16; bits = 0b1111111110111100; data = { run = 0x8; size = 0x9 } }
  ; { length = 16; bits = 0b1111111110111101; data = { run = 0x8; size = 0xA } }
  ; { length = 9; bits = 0b111111001; data = { run = 0x9; size = 0x1 } }
  ; { length = 16; bits = 0b1111111110111110; data = { run = 0x9; size = 0x2 } }
  ; { length = 16; bits = 0b1111111110111111; data = { run = 0x9; size = 0x3 } }
  ; { length = 16; bits = 0b1111111111000000; data = { run = 0x9; size = 0x4 } }
  ; { length = 16; bits = 0b1111111111000001; data = { run = 0x9; size = 0x5 } }
  ; { length = 16; bits = 0b1111111111000010; data = { run = 0x9; size = 0x6 } }
  ; { length = 16; bits = 0b1111111111000011; data = { run = 0x9; size = 0x7 } }
  ; { length = 16; bits = 0b1111111111000100; data = { run = 0x9; size = 0x8 } }
  ; { length = 16; bits = 0b1111111111000101; data = { run = 0x9; size = 0x9 } }
  ; { length = 16; bits = 0b1111111111000110; data = { run = 0x9; size = 0xA } }
  ; { length = 9; bits = 0b111111010; data = { run = 0xA; size = 0x1 } }
  ; { length = 16; bits = 0b1111111111000111; data = { run = 0xA; size = 0x2 } }
  ; { length = 16; bits = 0b1111111111001000; data = { run = 0xA; size = 0x3 } }
  ; { length = 16; bits = 0b1111111111001001; data = { run = 0xA; size = 0x4 } }
  ; { length = 16; bits = 0b1111111111001010; data = { run = 0xA; size = 0x5 } }
  ; { length = 16; bits = 0b1111111111001011; data = { run = 0xA; size = 0x6 } }
  ; { length = 16; bits = 0b1111111111001100; data = { run = 0xA; size = 0x7 } }
  ; { length = 16; bits = 0b1111111111001101; data = { run = 0xA; size = 0x8 } }
  ; { length = 16; bits = 0b1111111111001110; data = { run = 0xA; size = 0x9 } }
  ; { length = 16; bits = 0b1111111111001111; data = { run = 0xA; size = 0xA } }
  ; { length = 10; bits = 0b1111111001; data = { run = 0xB; size = 0x1 } }
  ; { length = 16; bits = 0b1111111111010000; data = { run = 0xB; size = 0x2 } }
  ; { length = 16; bits = 0b1111111111010001; data = { run = 0xB; size = 0x3 } }
  ; { length = 16; bits = 0b1111111111010010; data = { run = 0xB; size = 0x4 } }
  ; { length = 16; bits = 0b1111111111010011; data = { run = 0xB; size = 0x5 } }
  ; { length = 16; bits = 0b1111111111010100; data = { run = 0xB; size = 0x6 } }
  ; { length = 16; bits = 0b1111111111010101; data = { run = 0xB; size = 0x7 } }
  ; { length = 16; bits = 0b1111111111010110; data = { run = 0xB; size = 0x8 } }
  ; { length = 16; bits = 0b1111111111010111; data = { run = 0xB; size = 0x9 } }
  ; { length = 16; bits = 0b1111111111011000; data = { run = 0xB; size = 0xA } }
  ; { length = 10; bits = 0b1111111010; data = { run = 0xC; size = 0x1 } }
  ; { length = 16; bits = 0b1111111111011001; data = { run = 0xC; size = 0x2 } }
  ; { length = 16; bits = 0b1111111111011010; data = { run = 0xC; size = 0x3 } }
  ; { length = 16; bits = 0b1111111111011011; data = { run = 0xC; size = 0x4 } }
  ; { length = 16; bits = 0b1111111111011100; data = { run = 0xC; size = 0x5 } }
  ; { length = 16; bits = 0b1111111111011101; data = { run = 0xC; size = 0x6 } }
  ; { length = 16; bits = 0b1111111111011110; data = { run = 0xC; size = 0x7 } }
  ; { length = 16; bits = 0b1111111111011111; data = { run = 0xC; size = 0x8 } }
  ; { length = 16; bits = 0b1111111111100000; data = { run = 0xC; size = 0x9 } }
  ; { length = 16; bits = 0b1111111111100001; data = { run = 0xC; size = 0xA } }
  ; { length = 11; bits = 0b11111111000; data = { run = 0xD; size = 0x1 } }
  ; { length = 16; bits = 0b1111111111100010; data = { run = 0xD; size = 0x2 } }
  ; { length = 16; bits = 0b1111111111100011; data = { run = 0xD; size = 0x3 } }
  ; { length = 16; bits = 0b1111111111100100; data = { run = 0xD; size = 0x4 } }
  ; { length = 16; bits = 0b1111111111100101; data = { run = 0xD; size = 0x5 } }
  ; { length = 16; bits = 0b1111111111100110; data = { run = 0xD; size = 0x6 } }
  ; { length = 16; bits = 0b1111111111100111; data = { run = 0xD; size = 0x7 } }
  ; { length = 16; bits = 0b1111111111101000; data = { run = 0xD; size = 0x8 } }
  ; { length = 16; bits = 0b1111111111101001; data = { run = 0xD; size = 0x9 } }
  ; { length = 16; bits = 0b1111111111101010; data = { run = 0xD; size = 0xA } }
  ; { length = 16; bits = 0b1111111111101011; data = { run = 0xE; size = 0x1 } }
  ; { length = 16; bits = 0b1111111111101100; data = { run = 0xE; size = 0x2 } }
  ; { length = 16; bits = 0b1111111111101101; data = { run = 0xE; size = 0x3 } }
  ; { length = 16; bits = 0b1111111111101110; data = { run = 0xE; size = 0x4 } }
  ; { length = 16; bits = 0b1111111111101111; data = { run = 0xE; size = 0x5 } }
  ; { length = 16; bits = 0b1111111111110000; data = { run = 0xE; size = 0x6 } }
  ; { length = 16; bits = 0b1111111111110001; data = { run = 0xE; size = 0x7 } }
  ; { length = 16; bits = 0b1111111111110010; data = { run = 0xE; size = 0x8 } }
  ; { length = 16; bits = 0b1111111111110011; data = { run = 0xE; size = 0x9 } }
  ; { length = 16; bits = 0b1111111111110100; data = { run = 0xE; size = 0xA } }
  ; { length = 11; bits = 0b11111111001; data = { run = 0xF; size = 0x0 } }
  ; { length = 16; bits = 0b1111111111110101; data = { run = 0xF; size = 0x1 } }
  ; { length = 16; bits = 0b1111111111110110; data = { run = 0xF; size = 0x2 } }
  ; { length = 16; bits = 0b1111111111110111; data = { run = 0xF; size = 0x3 } }
  ; { length = 16; bits = 0b1111111111111000; data = { run = 0xF; size = 0x4 } }
  ; { length = 16; bits = 0b1111111111111001; data = { run = 0xF; size = 0x5 } }
  ; { length = 16; bits = 0b1111111111111010; data = { run = 0xF; size = 0x6 } }
  ; { length = 16; bits = 0b1111111111111011; data = { run = 0xF; size = 0x7 } }
  ; { length = 16; bits = 0b1111111111111100; data = { run = 0xF; size = 0x8 } }
  ; { length = 16; bits = 0b1111111111111101; data = { run = 0xF; size = 0x9 } }
  ; { length = 16; bits = 0b1111111111111110; data = { run = 0xF; size = 0xA } }
  ]
  |> sort
;;

let ac_chroma =
  [ { length = 2; bits = 0b00; data = { run = 0x0; size = 0x0 } }
  ; { length = 2; bits = 0b01; data = { run = 0x0; size = 0x1 } }
  ; { length = 3; bits = 0b100; data = { run = 0x0; size = 0x2 } }
  ; { length = 4; bits = 0b1010; data = { run = 0x0; size = 0x3 } }
  ; { length = 5; bits = 0b11000; data = { run = 0x0; size = 0x4 } }
  ; { length = 5; bits = 0b11001; data = { run = 0x0; size = 0x5 } }
  ; { length = 6; bits = 0b111000; data = { run = 0x0; size = 0x6 } }
  ; { length = 7; bits = 0b1111000; data = { run = 0x0; size = 0x7 } }
  ; { length = 9; bits = 0b111110100; data = { run = 0x0; size = 0x8 } }
  ; { length = 10; bits = 0b1111110110; data = { run = 0x0; size = 0x9 } }
  ; { length = 12; bits = 0b111111110100; data = { run = 0x0; size = 0xA } }
  ; { length = 4; bits = 0b1011; data = { run = 0x1; size = 0x1 } }
  ; { length = 6; bits = 0b111001; data = { run = 0x1; size = 0x2 } }
  ; { length = 8; bits = 0b11110110; data = { run = 0x1; size = 0x3 } }
  ; { length = 9; bits = 0b111110101; data = { run = 0x1; size = 0x4 } }
  ; { length = 11; bits = 0b11111110110; data = { run = 0x1; size = 0x5 } }
  ; { length = 12; bits = 0b111111110101; data = { run = 0x1; size = 0x6 } }
  ; { length = 16; bits = 0b1111111110001000; data = { run = 0x1; size = 0x7 } }
  ; { length = 16; bits = 0b1111111110001001; data = { run = 0x1; size = 0x8 } }
  ; { length = 16; bits = 0b1111111110001010; data = { run = 0x1; size = 0x9 } }
  ; { length = 16; bits = 0b1111111110001011; data = { run = 0x1; size = 0xA } }
  ; { length = 5; bits = 0b11010; data = { run = 0x2; size = 0x1 } }
  ; { length = 8; bits = 0b11110111; data = { run = 0x2; size = 0x2 } }
  ; { length = 10; bits = 0b1111110111; data = { run = 0x2; size = 0x3 } }
  ; { length = 12; bits = 0b111111110110; data = { run = 0x2; size = 0x4 } }
  ; { length = 15; bits = 0b111111111000010; data = { run = 0x2; size = 0x5 } }
  ; { length = 16; bits = 0b1111111110001100; data = { run = 0x2; size = 0x6 } }
  ; { length = 16; bits = 0b1111111110001101; data = { run = 0x2; size = 0x7 } }
  ; { length = 16; bits = 0b1111111110001110; data = { run = 0x2; size = 0x8 } }
  ; { length = 16; bits = 0b1111111110001111; data = { run = 0x2; size = 0x9 } }
  ; { length = 16; bits = 0b1111111110010000; data = { run = 0x2; size = 0xA } }
  ; { length = 5; bits = 0b11011; data = { run = 0x3; size = 0x1 } }
  ; { length = 8; bits = 0b11111000; data = { run = 0x3; size = 0x2 } }
  ; { length = 10; bits = 0b1111111000; data = { run = 0x3; size = 0x3 } }
  ; { length = 12; bits = 0b111111110111; data = { run = 0x3; size = 0x4 } }
  ; { length = 16; bits = 0b1111111110010001; data = { run = 0x3; size = 0x5 } }
  ; { length = 16; bits = 0b1111111110010010; data = { run = 0x3; size = 0x6 } }
  ; { length = 16; bits = 0b1111111110010011; data = { run = 0x3; size = 0x7 } }
  ; { length = 16; bits = 0b1111111110010100; data = { run = 0x3; size = 0x8 } }
  ; { length = 16; bits = 0b1111111110010101; data = { run = 0x3; size = 0x9 } }
  ; { length = 16; bits = 0b1111111110010110; data = { run = 0x3; size = 0xA } }
  ; { length = 6; bits = 0b111010; data = { run = 0x4; size = 0x1 } }
  ; { length = 9; bits = 0b111110110; data = { run = 0x4; size = 0x2 } }
  ; { length = 16; bits = 0b1111111110010111; data = { run = 0x4; size = 0x3 } }
  ; { length = 16; bits = 0b1111111110011000; data = { run = 0x4; size = 0x4 } }
  ; { length = 16; bits = 0b1111111110011001; data = { run = 0x4; size = 0x5 } }
  ; { length = 16; bits = 0b1111111110011010; data = { run = 0x4; size = 0x6 } }
  ; { length = 16; bits = 0b1111111110011011; data = { run = 0x4; size = 0x7 } }
  ; { length = 16; bits = 0b1111111110011100; data = { run = 0x4; size = 0x8 } }
  ; { length = 16; bits = 0b1111111110011101; data = { run = 0x4; size = 0x9 } }
  ; { length = 16; bits = 0b1111111110011110; data = { run = 0x4; size = 0xA } }
  ; { length = 6; bits = 0b111011; data = { run = 0x5; size = 0x1 } }
  ; { length = 10; bits = 0b1111111001; data = { run = 0x5; size = 0x2 } }
  ; { length = 16; bits = 0b1111111110011111; data = { run = 0x5; size = 0x3 } }
  ; { length = 16; bits = 0b1111111110100000; data = { run = 0x5; size = 0x4 } }
  ; { length = 16; bits = 0b1111111110100001; data = { run = 0x5; size = 0x5 } }
  ; { length = 16; bits = 0b1111111110100010; data = { run = 0x5; size = 0x6 } }
  ; { length = 16; bits = 0b1111111110100011; data = { run = 0x5; size = 0x7 } }
  ; { length = 16; bits = 0b1111111110100100; data = { run = 0x5; size = 0x8 } }
  ; { length = 16; bits = 0b1111111110100101; data = { run = 0x5; size = 0x9 } }
  ; { length = 16; bits = 0b1111111110100110; data = { run = 0x5; size = 0xA } }
  ; { length = 7; bits = 0b1111001; data = { run = 0x6; size = 0x1 } }
  ; { length = 11; bits = 0b11111110111; data = { run = 0x6; size = 0x2 } }
  ; { length = 16; bits = 0b1111111110100111; data = { run = 0x6; size = 0x3 } }
  ; { length = 16; bits = 0b1111111110101000; data = { run = 0x6; size = 0x4 } }
  ; { length = 16; bits = 0b1111111110101001; data = { run = 0x6; size = 0x5 } }
  ; { length = 16; bits = 0b1111111110101010; data = { run = 0x6; size = 0x6 } }
  ; { length = 16; bits = 0b1111111110101011; data = { run = 0x6; size = 0x7 } }
  ; { length = 16; bits = 0b1111111110101100; data = { run = 0x6; size = 0x8 } }
  ; { length = 16; bits = 0b1111111110101101; data = { run = 0x6; size = 0x9 } }
  ; { length = 16; bits = 0b1111111110101110; data = { run = 0x6; size = 0xA } }
  ; { length = 7; bits = 0b1111010; data = { run = 0x7; size = 0x1 } }
  ; { length = 11; bits = 0b11111111000; data = { run = 0x7; size = 0x2 } }
  ; { length = 16; bits = 0b1111111110101111; data = { run = 0x7; size = 0x3 } }
  ; { length = 16; bits = 0b1111111110110000; data = { run = 0x7; size = 0x4 } }
  ; { length = 16; bits = 0b1111111110110001; data = { run = 0x7; size = 0x5 } }
  ; { length = 16; bits = 0b1111111110110010; data = { run = 0x7; size = 0x6 } }
  ; { length = 16; bits = 0b1111111110110011; data = { run = 0x7; size = 0x7 } }
  ; { length = 16; bits = 0b1111111110110100; data = { run = 0x7; size = 0x8 } }
  ; { length = 16; bits = 0b1111111110110101; data = { run = 0x7; size = 0x9 } }
  ; { length = 16; bits = 0b1111111110110110; data = { run = 0x7; size = 0xA } }
  ; { length = 8; bits = 0b11111001; data = { run = 0x8; size = 0x1 } }
  ; { length = 16; bits = 0b1111111110110111; data = { run = 0x8; size = 0x2 } }
  ; { length = 16; bits = 0b1111111110111000; data = { run = 0x8; size = 0x3 } }
  ; { length = 16; bits = 0b1111111110111001; data = { run = 0x8; size = 0x4 } }
  ; { length = 16; bits = 0b1111111110111010; data = { run = 0x8; size = 0x5 } }
  ; { length = 16; bits = 0b1111111110111011; data = { run = 0x8; size = 0x6 } }
  ; { length = 16; bits = 0b1111111110111100; data = { run = 0x8; size = 0x7 } }
  ; { length = 16; bits = 0b1111111110111101; data = { run = 0x8; size = 0x8 } }
  ; { length = 16; bits = 0b1111111110111110; data = { run = 0x8; size = 0x9 } }
  ; { length = 16; bits = 0b1111111110111111; data = { run = 0x8; size = 0xA } }
  ; { length = 9; bits = 0b111110111; data = { run = 0x9; size = 0x1 } }
  ; { length = 16; bits = 0b1111111111000000; data = { run = 0x9; size = 0x2 } }
  ; { length = 16; bits = 0b1111111111000001; data = { run = 0x9; size = 0x3 } }
  ; { length = 16; bits = 0b1111111111000010; data = { run = 0x9; size = 0x4 } }
  ; { length = 16; bits = 0b1111111111000011; data = { run = 0x9; size = 0x5 } }
  ; { length = 16; bits = 0b1111111111000100; data = { run = 0x9; size = 0x6 } }
  ; { length = 16; bits = 0b1111111111000101; data = { run = 0x9; size = 0x7 } }
  ; { length = 16; bits = 0b1111111111000110; data = { run = 0x9; size = 0x8 } }
  ; { length = 16; bits = 0b1111111111000111; data = { run = 0x9; size = 0x9 } }
  ; { length = 16; bits = 0b1111111111001000; data = { run = 0x9; size = 0xA } }
  ; { length = 9; bits = 0b111111000; data = { run = 0xA; size = 0x1 } }
  ; { length = 16; bits = 0b1111111111001001; data = { run = 0xA; size = 0x2 } }
  ; { length = 16; bits = 0b1111111111001010; data = { run = 0xA; size = 0x3 } }
  ; { length = 16; bits = 0b1111111111001011; data = { run = 0xA; size = 0x4 } }
  ; { length = 16; bits = 0b1111111111001100; data = { run = 0xA; size = 0x5 } }
  ; { length = 16; bits = 0b1111111111001101; data = { run = 0xA; size = 0x6 } }
  ; { length = 16; bits = 0b1111111111001110; data = { run = 0xA; size = 0x7 } }
  ; { length = 16; bits = 0b1111111111001111; data = { run = 0xA; size = 0x8 } }
  ; { length = 16; bits = 0b1111111111010000; data = { run = 0xA; size = 0x9 } }
  ; { length = 16; bits = 0b1111111111010001; data = { run = 0xA; size = 0xA } }
  ; { length = 9; bits = 0b111111001; data = { run = 0xB; size = 0x1 } }
  ; { length = 16; bits = 0b1111111111010010; data = { run = 0xB; size = 0x2 } }
  ; { length = 16; bits = 0b1111111111010011; data = { run = 0xB; size = 0x3 } }
  ; { length = 16; bits = 0b1111111111010100; data = { run = 0xB; size = 0x4 } }
  ; { length = 16; bits = 0b1111111111010101; data = { run = 0xB; size = 0x5 } }
  ; { length = 16; bits = 0b1111111111010110; data = { run = 0xB; size = 0x6 } }
  ; { length = 16; bits = 0b1111111111010111; data = { run = 0xB; size = 0x7 } }
  ; { length = 16; bits = 0b1111111111011000; data = { run = 0xB; size = 0x8 } }
  ; { length = 16; bits = 0b1111111111011001; data = { run = 0xB; size = 0x9 } }
  ; { length = 16; bits = 0b1111111111011010; data = { run = 0xB; size = 0xA } }
  ; { length = 9; bits = 0b111111010; data = { run = 0xC; size = 0x1 } }
  ; { length = 16; bits = 0b1111111111011011; data = { run = 0xC; size = 0x2 } }
  ; { length = 16; bits = 0b1111111111011100; data = { run = 0xC; size = 0x3 } }
  ; { length = 16; bits = 0b1111111111011101; data = { run = 0xC; size = 0x4 } }
  ; { length = 16; bits = 0b1111111111011110; data = { run = 0xC; size = 0x5 } }
  ; { length = 16; bits = 0b1111111111011111; data = { run = 0xC; size = 0x6 } }
  ; { length = 16; bits = 0b1111111111100000; data = { run = 0xC; size = 0x7 } }
  ; { length = 16; bits = 0b1111111111100001; data = { run = 0xC; size = 0x8 } }
  ; { length = 16; bits = 0b1111111111100010; data = { run = 0xC; size = 0x9 } }
  ; { length = 16; bits = 0b1111111111100011; data = { run = 0xC; size = 0xA } }
  ; { length = 11; bits = 0b11111111001; data = { run = 0xD; size = 0x1 } }
  ; { length = 16; bits = 0b1111111111100100; data = { run = 0xD; size = 0x2 } }
  ; { length = 16; bits = 0b1111111111100101; data = { run = 0xD; size = 0x3 } }
  ; { length = 16; bits = 0b1111111111100110; data = { run = 0xD; size = 0x4 } }
  ; { length = 16; bits = 0b1111111111100111; data = { run = 0xD; size = 0x5 } }
  ; { length = 16; bits = 0b1111111111101000; data = { run = 0xD; size = 0x6 } }
  ; { length = 16; bits = 0b1111111111101001; data = { run = 0xD; size = 0x7 } }
  ; { length = 16; bits = 0b1111111111101010; data = { run = 0xD; size = 0x8 } }
  ; { length = 16; bits = 0b1111111111101011; data = { run = 0xD; size = 0x9 } }
  ; { length = 16; bits = 0b1111111111101100; data = { run = 0xD; size = 0xA } }
  ; { length = 14; bits = 0b11111111100000; data = { run = 0xE; size = 0x1 } }
  ; { length = 16; bits = 0b1111111111101101; data = { run = 0xE; size = 0x2 } }
  ; { length = 16; bits = 0b1111111111101110; data = { run = 0xE; size = 0x3 } }
  ; { length = 16; bits = 0b1111111111101111; data = { run = 0xE; size = 0x4 } }
  ; { length = 16; bits = 0b1111111111110000; data = { run = 0xE; size = 0x5 } }
  ; { length = 16; bits = 0b1111111111110001; data = { run = 0xE; size = 0x6 } }
  ; { length = 16; bits = 0b1111111111110010; data = { run = 0xE; size = 0x7 } }
  ; { length = 16; bits = 0b1111111111110011; data = { run = 0xE; size = 0x8 } }
  ; { length = 16; bits = 0b1111111111110100; data = { run = 0xE; size = 0x9 } }
  ; { length = 16; bits = 0b1111111111110101; data = { run = 0xE; size = 0xA } }
  ; { length = 10; bits = 0b1111111010; data = { run = 0xF; size = 0x0 } }
  ; { length = 15; bits = 0b111111111000011; data = { run = 0xF; size = 0x1 } }
  ; { length = 16; bits = 0b1111111111110110; data = { run = 0xF; size = 0x2 } }
  ; { length = 16; bits = 0b1111111111110111; data = { run = 0xF; size = 0x3 } }
  ; { length = 16; bits = 0b1111111111111000; data = { run = 0xF; size = 0x4 } }
  ; { length = 16; bits = 0b1111111111111001; data = { run = 0xF; size = 0x5 } }
  ; { length = 16; bits = 0b1111111111111010; data = { run = 0xF; size = 0x6 } }
  ; { length = 16; bits = 0b1111111111111011; data = { run = 0xF; size = 0x7 } }
  ; { length = 16; bits = 0b1111111111111100; data = { run = 0xF; size = 0x8 } }
  ; { length = 16; bits = 0b1111111111111101; data = { run = 0xF; size = 0x9 } }
  ; { length = 16; bits = 0b1111111111111110; data = { run = 0xF; size = 0xA } }
  ]
  |> sort
;;

module Lut = struct
  type 'a code =
    { length : int
    ; data : 'a
    }

  type 'a t =
    { lut : 'a code option array
    ; max_bits : int
    }
  [@@deriving fields]

  let create (codes : 'a coef list) =
    let max_bits = List.fold codes ~init:0 ~f:(fun a b -> max a b.length) in
    let lut = Array.create ~len:(1 lsl max_bits) None in
    List.iter codes ~f:(fun code ->
        let null_bits = max_bits - code.length in
        let first = code.bits lsl null_bits in
        let count = 1 lsl null_bits in
        for i = first to first + count - 1 do
          lut.(i) <- Some { length = code.length; data = code.data }
        done);
    { lut; max_bits }
  ;;
end

module Encoder = struct
  let dc_table (dc : dc coef list) =
    List.sort
      dc
      ~compare:(fun
                 { length = _; bits = _; data = data0 }
                 { length = _; bits = _; data = data1 }
               -> Int.compare data0 data1)
    |> Array.of_list
  ;;

  let ac_table (ac : ac coef list) =
    let ac =
      List.sort
        ac
        ~compare:(fun
                   { length = _; bits = _; data = { run = run0; size = size0 } }
                   { length = _; bits = _; data = { run = run1; size = size1 } }
                 -> [%compare: int * int] (run0, size0) (run1, size1))
    in
    let by_run =
      List.group
        ac
        ~break:(fun
                 { length = _; bits = _; data = { run = run0; size = _ } }
                 { length = _; bits = _; data = { run = run1; size = _ } }
               -> run0 <> run1)
    in
    List.map by_run ~f:(fun l ->
        (* If there is no [0] size, add a fake one back in to normalize indexing. 
        
          run=0 and run=15 implicitly have one and are used for end of block and extended 0 runs.
        *)
        match l with
        | [] -> failwith ""
        | { length = _; bits = _; data = { run = _; size = 0 } } :: _ -> Array.of_list l
        | _ -> Array.of_list ({ length = 0; bits = 0; data = { run = 0; size = 0 } } :: l))
    |> Array.of_list
  ;;
end
