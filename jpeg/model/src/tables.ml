open Base

type 'a coef =
  { length : int
  ; bits : int
  ; data : 'a
  }
[@@deriving sexp_of]

type dc = int [@@deriving sexp_of]
type dc_coef = dc coef [@@deriving sexp_of]

type ac =
  { run : int
  ; size : int
  }
[@@deriving sexp_of]

type ac_coef = ac coef [@@deriving sexp_of]

module Specification = struct
  type t =
    { lengths : int array
    ; values : int array
    }

  let create_code_table { lengths; values } ~to_data =
    let rec build code length_pos data_pos =
      if length_pos = Array.length lengths
      then []
      else if lengths.(length_pos) = 0
      then build (code lsl 1) (length_pos + 1) data_pos
      else
        List.init lengths.(length_pos) ~f:(fun i ->
            { length = length_pos + 1
            ; bits = code + i
            ; data = to_data values.(data_pos + i)
            })
        :: build
             ((code + lengths.(length_pos)) lsl 1)
             (length_pos + 1)
             (data_pos + lengths.(length_pos))
    in
    build 0 0 0 |> List.concat
  ;;

  let create_dc_code_table = create_code_table ~to_data:Fn.id

  let create_ac_code_table =
    create_code_table ~to_data:(fun d -> { run = (d lsr 4) land 0xf; size = d land 0xf })
  ;;
end

module Default = struct
  open Specification

  let dc_luma =
    { lengths =
        [| 0x00
         ; 0x01
         ; 0x05
         ; 0x01
         ; 0x01
         ; 0x01
         ; 0x01
         ; 0x01
         ; 0x01
         ; 0x00
         ; 0x00
         ; 0x00
         ; 0x00
         ; 0x00
         ; 0x00
         ; 0x00
        |]
    ; values =
        [| 0x00; 0x01; 0x02; 0x03; 0x04; 0x05; 0x06; 0x07; 0x08; 0x09; 0x0A; 0x0B |]
    }
  ;;

  let dc_chroma =
    { lengths =
        [| 0x00
         ; 0x03
         ; 0x01
         ; 0x01
         ; 0x01
         ; 0x01
         ; 0x01
         ; 0x01
         ; 0x01
         ; 0x01
         ; 0x01
         ; 0x00
         ; 0x00
         ; 0x00
         ; 0x00
         ; 0x00
        |]
    ; values =
        [| 0x00; 0x01; 0x02; 0x03; 0x04; 0x05; 0x06; 0x07; 0x08; 0x09; 0x0A; 0x0B |]
    }
  ;;

  let ac_luma =
    { lengths =
        [| 0x00
         ; 0x02
         ; 0x01
         ; 0x03
         ; 0x03
         ; 0x02
         ; 0x04
         ; 0x03
         ; 0x05
         ; 0x05
         ; 0x04
         ; 0x04
         ; 0x00
         ; 0x00
         ; 0x01
         ; 0x7D
        |]
    ; values =
        [| 0x01
         ; 0x02
         ; 0x03
         ; 0x00
         ; 0x04
         ; 0x11
         ; 0x05
         ; 0x12
         ; 0x21
         ; 0x31
         ; 0x41
         ; 0x06
         ; 0x13
         ; 0x51
         ; 0x61
         ; 0x07
         ; 0x22
         ; 0x71
         ; 0x14
         ; 0x32
         ; 0x81
         ; 0x91
         ; 0xA1
         ; 0x08
         ; 0x23
         ; 0x42
         ; 0xB1
         ; 0xC1
         ; 0x15
         ; 0x52
         ; 0xD1
         ; 0xF0
         ; 0x24
         ; 0x33
         ; 0x62
         ; 0x72
         ; 0x82
         ; 0x09
         ; 0x0A
         ; 0x16
         ; 0x17
         ; 0x18
         ; 0x19
         ; 0x1A
         ; 0x25
         ; 0x26
         ; 0x27
         ; 0x28
         ; 0x29
         ; 0x2A
         ; 0x34
         ; 0x35
         ; 0x36
         ; 0x37
         ; 0x38
         ; 0x39
         ; 0x3A
         ; 0x43
         ; 0x44
         ; 0x45
         ; 0x46
         ; 0x47
         ; 0x48
         ; 0x49
         ; 0x4A
         ; 0x53
         ; 0x54
         ; 0x55
         ; 0x56
         ; 0x57
         ; 0x58
         ; 0x59
         ; 0x5A
         ; 0x63
         ; 0x64
         ; 0x65
         ; 0x66
         ; 0x67
         ; 0x68
         ; 0x69
         ; 0x6A
         ; 0x73
         ; 0x74
         ; 0x75
         ; 0x76
         ; 0x77
         ; 0x78
         ; 0x79
         ; 0x7A
         ; 0x83
         ; 0x84
         ; 0x85
         ; 0x86
         ; 0x87
         ; 0x88
         ; 0x89
         ; 0x8A
         ; 0x92
         ; 0x93
         ; 0x94
         ; 0x95
         ; 0x96
         ; 0x97
         ; 0x98
         ; 0x99
         ; 0x9A
         ; 0xA2
         ; 0xA3
         ; 0xA4
         ; 0xA5
         ; 0xA6
         ; 0xA7
         ; 0xA8
         ; 0xA9
         ; 0xAA
         ; 0xB2
         ; 0xB3
         ; 0xB4
         ; 0xB5
         ; 0xB6
         ; 0xB7
         ; 0xB8
         ; 0xB9
         ; 0xBA
         ; 0xC2
         ; 0xC3
         ; 0xC4
         ; 0xC5
         ; 0xC6
         ; 0xC7
         ; 0xC8
         ; 0xC9
         ; 0xCA
         ; 0xD2
         ; 0xD3
         ; 0xD4
         ; 0xD5
         ; 0xD6
         ; 0xD7
         ; 0xD8
         ; 0xD9
         ; 0xDA
         ; 0xE1
         ; 0xE2
         ; 0xE3
         ; 0xE4
         ; 0xE5
         ; 0xE6
         ; 0xE7
         ; 0xE8
         ; 0xE9
         ; 0xEA
         ; 0xF1
         ; 0xF2
         ; 0xF3
         ; 0xF4
         ; 0xF5
         ; 0xF6
         ; 0xF7
         ; 0xF8
         ; 0xF9
         ; 0xFA
        |]
    }
  ;;

  let ac_chroma =
    { lengths =
        [| 0x00
         ; 0x02
         ; 0x01
         ; 0x02
         ; 0x04
         ; 0x04
         ; 0x03
         ; 0x04
         ; 0x07
         ; 0x05
         ; 0x04
         ; 0x04
         ; 0x00
         ; 0x01
         ; 0x02
         ; 0x77
        |]
    ; values =
        [| 0x00
         ; 0x01
         ; 0x02
         ; 0x03
         ; 0x11
         ; 0x04
         ; 0x05
         ; 0x21
         ; 0x31
         ; 0x06
         ; 0x12
         ; 0x41
         ; 0x51
         ; 0x07
         ; 0x61
         ; 0x71
         ; 0x13
         ; 0x22
         ; 0x32
         ; 0x81
         ; 0x08
         ; 0x14
         ; 0x42
         ; 0x91
         ; 0xA1
         ; 0xB1
         ; 0xC1
         ; 0x09
         ; 0x23
         ; 0x33
         ; 0x52
         ; 0xF0
         ; 0x15
         ; 0x62
         ; 0x72
         ; 0xD1
         ; 0x0A
         ; 0x16
         ; 0x24
         ; 0x34
         ; 0xE1
         ; 0x25
         ; 0xF1
         ; 0x17
         ; 0x18
         ; 0x19
         ; 0x1A
         ; 0x26
         ; 0x27
         ; 0x28
         ; 0x29
         ; 0x2A
         ; 0x35
         ; 0x36
         ; 0x37
         ; 0x38
         ; 0x39
         ; 0x3A
         ; 0x43
         ; 0x44
         ; 0x45
         ; 0x46
         ; 0x47
         ; 0x48
         ; 0x49
         ; 0x4A
         ; 0x53
         ; 0x54
         ; 0x55
         ; 0x56
         ; 0x57
         ; 0x58
         ; 0x59
         ; 0x5A
         ; 0x63
         ; 0x64
         ; 0x65
         ; 0x66
         ; 0x67
         ; 0x68
         ; 0x69
         ; 0x6A
         ; 0x73
         ; 0x74
         ; 0x75
         ; 0x76
         ; 0x77
         ; 0x78
         ; 0x79
         ; 0x7A
         ; 0x82
         ; 0x83
         ; 0x84
         ; 0x85
         ; 0x86
         ; 0x87
         ; 0x88
         ; 0x89
         ; 0x8A
         ; 0x92
         ; 0x93
         ; 0x94
         ; 0x95
         ; 0x96
         ; 0x97
         ; 0x98
         ; 0x99
         ; 0x9A
         ; 0xA2
         ; 0xA3
         ; 0xA4
         ; 0xA5
         ; 0xA6
         ; 0xA7
         ; 0xA8
         ; 0xA9
         ; 0xAA
         ; 0xB2
         ; 0xB3
         ; 0xB4
         ; 0xB5
         ; 0xB6
         ; 0xB7
         ; 0xB8
         ; 0xB9
         ; 0xBA
         ; 0xC2
         ; 0xC3
         ; 0xC4
         ; 0xC5
         ; 0xC6
         ; 0xC7
         ; 0xC8
         ; 0xC9
         ; 0xCA
         ; 0xD2
         ; 0xD3
         ; 0xD4
         ; 0xD5
         ; 0xD6
         ; 0xD7
         ; 0xD8
         ; 0xD9
         ; 0xDA
         ; 0xE2
         ; 0xE3
         ; 0xE4
         ; 0xE5
         ; 0xE6
         ; 0xE7
         ; 0xE8
         ; 0xE9
         ; 0xEA
         ; 0xF2
         ; 0xF3
         ; 0xF4
         ; 0xF5
         ; 0xF6
         ; 0xF7
         ; 0xF8
         ; 0xF9
         ; 0xFA
        |]
    }
  ;;
end

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
  let dc_table dc =
    let dc = Specification.create_dc_code_table dc in
    List.sort
      dc
      ~compare:(fun
                 { length = _; bits = _; data = data0 }
                 { length = _; bits = _; data = data1 }
               -> Int.compare data0 data1)
    |> Array.of_list
  ;;

  let ac_table ac =
    let ac = Specification.create_ac_code_table ac in
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
