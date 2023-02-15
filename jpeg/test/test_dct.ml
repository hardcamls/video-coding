(* Test forward and inverse DCTs. *)
open Core
open Hardcaml
open Hardcaml_waveterm
module Dct = Hardcaml_jpeg.Dct

(* 
  fdct
    - input 8 bits 
    - rom 12 bits
    - (12+8+3)=23 bit mac
    - 4 bit transpose fixed point -> (8+3+4)=15 bits
  - 2nd stage
    - input 15 bits 
    - rom 12 bits 
    - (12+15+3)=30 bits mac -> fixed point @ (12+4)=16 bits
    - output clipped to 12 bits

  idct
    - input 12 bits
    - rom 16 bits
    - (16+12+3)=31 bits
    - 4 bit transpose fixed point -> (31-16+4)=19 bits
  - 2nd stage 
    - input 19 bits 
    - rom 16 bits
    - (19+16+3)=38 bits mac -> fixed point @ (16+4)=20 bits
    - output clipped to 8 bits
*)

let%expect_test "inverse matrix, scaled" =
  let m = Hardcaml_jpeg_model.Dct.Floating_point.Eight_point.inverse_transform_matrix in
  let m =
    Array.map
      m
      ~f:(Array.map ~f:(fun f -> Float.(f * (2. ** of_int 12) |> round_nearest)))
  in
  print_s [%message (m : float array array)];
  [%expect
    {|
    (m
     ((1448 2009 1892 1703 1448 1138 784 400)
      (1448 1703 784 -400 -1448 -2009 -1892 -1138)
      (1448 1138 -784 -2009 -1448 400 1892 1703)
      (1448 400 -1892 -1138 1448 1703 -784 -2009)
      (1448 -400 -1892 1138 1448 -1703 -784 2009)
      (1448 -1138 -784 2009 -1448 -400 1892 -1703)
      (1448 -1703 784 400 -1448 2009 -1892 1138)
      (1448 -2009 1892 -1703 1448 -1138 784 -400))) |}]
;;

let display_rules =
  let module I = Display_rules.With_interface (Dct.I) in
  let module O = Display_rules.With_interface (Dct.O) in
  List.concat
    [ I.default ~wave_format:Int ()
    ; O.default ~wave_format:Int ()
    ; Display_rule.
        [ port_name_is "dct_coef" ~wave_format:Int
        ; port_name_is "dct_mul" ~wave_format:Int
        ; port_name_is "dct_mac" ~wave_format:Int
        ; port_name_is "x" ~wave_format:Unsigned_int
        ; port_name_is "y" ~wave_format:Unsigned_int
        ; port_name_is "z" ~wave_format:Unsigned_int
        ; port_name_is "pass" ~wave_format:Bit
        ]
    ]
;;

let create_inputs () =
  let inputs =
    Array.init 8 ~f:(fun _ -> Array.init 8 ~f:(fun _ -> Random.int 400 - 200))
  in
  inputs
;;

let simulate_idct idct_inputs =
  let module Sim = Cyclesim.With_interface (Dct.I) (Dct.O) in
  let scope = Scope.create ~flatten_design:true () in
  let sim = Sim.create ~config:Cyclesim.Config.trace_all (Dct.create scope) in
  let waves, sim = Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let transpose = Array.init 8 ~f:(fun _ -> Array.init 8 ~f:(fun _ -> 0)) in
  let pixels = Array.init 8 ~f:(fun _ -> Array.init 8 ~f:(fun _ -> 0)) in
  let cycle () =
    let waddr = !(outputs.write_address) in
    (if Bits.to_bool !(outputs.transpose_write)
    then
      Bits.(
        transpose.(Bits.to_int waddr.:[5, 3]).(Bits.to_int waddr.:[2, 0])
          <- Bits.to_sint !(outputs.transpose_coef_out)));
    (if Bits.to_bool !(outputs.pixel_write)
    then
      Bits.(
        pixels.(Bits.to_int waddr.:[5, 3]).(Bits.to_int waddr.:[2, 0])
          <- Bits.to_sint !(outputs.pixel)));
    let raddr = !(outputs.read_address) in
    let renc = !(outputs.coef_read) in
    let rent = !(outputs.transpose_read) in
    Cyclesim.cycle sim;
    let row, col = Bits.(to_int raddr.:[5, 3]), Bits.(to_int raddr.:[2, 0]) in
    if Bits.to_bool renc
    then inputs.coef := Bits.of_int ~width:Dct.input_bits idct_inputs.(row).(col);
    if Bits.to_bool rent
    then
      inputs.transpose_coef_in
        := Bits.of_int ~width:Dct.transpose_bits transpose.(row).(col)
  in
  inputs.start := Bits.vdd;
  cycle ();
  inputs.start := Bits.gnd;
  for _ = 0 to 1200 do
    cycle ()
  done;
  print_s
    [%message
      (idct_inputs : int array array)
        (transpose : int array array)
        (pixels : int array array)];
  waves
;;

let reference idct_inputs =
  let open Hardcaml_jpeg_model.Dct in
  let coefs =
    Fixed_point.fixed_coefs
      Floating_point.Eight_point.inverse_transform_matrix
      ~fixed_prec:12
  in
  let transpose = Matrix8x8.imul coefs idct_inputs |> Fixed_point.round_matrix ~prec:8 in
  print_s [%message (transpose : int Matrix8x8.t)];
  let unscaled_result = Matrix8x8.imul transpose (Matrix8x8.transpose coefs) in
  let result = Fixed_point.round_matrix unscaled_result ~prec:16 in
  let result = Matrix8x8.iclip ~min:(-128) ~max:127 result in
  print_s [%message (result : int Matrix8x8.t)]
;;

let%expect_test "test idct" =
  let idct_inputs = create_inputs () in
  reference idct_inputs;
  [%expect
    {|
    (transpose
     ((-1976 852 -1195 -152 -587 -2373 -2576 -785)
      (61 2948 -232 416 275 1295 -2616 -3121)
      (-752 2377 86 -12 -2946 1026 409 -424)
      (1437 -2720 -4098 -1455 -284 -1025 3033 -548)
      (1199 832 -1374 -1232 3817 2221 -2883 -2801)
      (-658 1237 -787 3463 -304 793 -290 -2428)
      (-1456 180 2132 817 -1075 596 -1350 -815)
      (969 267 -505 2228 334 317 -1782 2007)))
    (result
     ((-128 127 -106 -21 38 -75 -80 -98) (74 127 -112 127 -95 -43 -21 -128)
      (13 75 96 -35 -128 22 0 -128) (-128 -119 127 106 109 127 -78 86)
      (52 19 -128 127 15 -118 -1 22) (79 20 -128 47 -38 127 -38 -128)
      (22 52 -112 -74 -128 -33 60 -43) (77 8 -41 -26 127 -22 111 -91))) |}];
  let waves = simulate_idct idct_inputs in
  Waveform.print
    ~display_width:190
    ~display_height:60
    ~wave_width:4
    ~start_cycle:(8 * 64)
    ~display_rules
    waves;
  [%expect
    {|
    ((idct_inputs
      ((-26 132 -132 90 -17 63 -178 -197) (-51 88 -84 -145 -64 -80 -9 -97)
       (-105 81 140 111 -80 -93 -179 119) (-87 25 17 51 120 -25 -137 -159)
       (98 -166 -185 -117 162 -101 -8 103) (-92 -160 -5 -56 -180 -151 183 93)
       (-44 50 -30 121 -116 -41 64 86) (-54 94 143 -91 28 77 -146 6)))
     (transpose
      ((-1976 852 -1195 -152 -587 -2373 -2576 -785)
       (61 2948 -232 416 275 1295 -2616 -3121)
       (-752 2377 86 -12 -2946 1026 409 -424)
       (1437 -2720 -4098 -1455 -284 -1025 3033 -548)
       (1199 832 -1374 -1232 3817 2221 -2883 -2801)
       (-658 1237 -787 3463 -304 793 -290 -2428)
       (-1456 180 2132 817 -1075 596 -1350 -815)
       (969 267 -505 2228 334 317 -1782 2007)))
     (pixels
      ((-128 127 -106 -21 38 -75 -80 -98) (74 127 -112 127 -95 -43 -21 -128)
       (13 75 96 -35 -128 22 0 -128) (-128 -119 127 106 109 127 -78 86)
       (52 19 -128 127 15 -118 -1 22) (79 20 -128 47 -38 127 -38 -128)
       (22 52 -112 -74 -128 -33 60 -43) (77 8 -41 -26 127 -22 111 -91))))
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
    │clock             ││┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐  │
    │                  ││     └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └──│
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │clear             ││ 0                                                                                                                                                                      │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │start             ││ 0                                                                                                                                                                      │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││──────────┬─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │coef              ││ 86       │6                                                                                                                                                            │
    │                  ││──────────┴─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││────────────────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬───────│
    │transpose_coef_in ││ 0                  │-1976    │852      │-1195    │-152     │-587     │-2373    │-2576    │-785     │-1976    │852      │-1195    │-152     │-587     │-2373    │-2576  │
    │                  ││────────────────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴───────│
    │                  ││──────────┬─────────┬───────────────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬───────────────────┬─────────┬─────────┬─────────┬─────────┬───────│
    │pixel             ││ 8        │7        │8                  │-44      │-18      │-52      │-56      │-69      │-110     │-128               │-44      │-22      │-36      │-35      │-22    │
    │                  ││──────────┴─────────┴───────────────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴───────────────────┴─────────┴─────────┴─────────┴─────────┴───────│
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────────────────┬─────────┬───────────────────────────────────────────────│
    │pixel_write       ││ 0                                                                                                            │-1       │0                                              │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────────────────┴─────────┴───────────────────────────────────────────────│
    │                  ││──────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬───────│
    │transpose_coef_out││ 2167     │1753     │2017     │2007     │-11177   │-4491    │-13322   │-14334   │-17654   │-28202   │-36091   │-37318   │-11177   │-5509    │-9169    │-8931    │-5611  │
    │                  ││──────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴───────│
    │                  ││──────────────────────────────┬─────────┬───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │transpose_write   ││ 0                            │-1       │0                                                                                                                              │
    │                  ││──────────────────────────────┴─────────┴───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││──────────┬─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │coef_read         ││ -1       │0                                                                                                                                                            │
    │                  ││──────────┴─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││──────────┬─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │transpose_read    ││ 0        │-1                                                                                                                                                           │
    │                  ││──────────┴─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││──────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬───────│
    │read_address      ││ -1       │0        │1        │2        │3        │4        │5        │6        │7        │0        │1        │2        │3        │4        │5        │6        │7      │
    │                  ││──────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴───────│
    │                  ││────────────────────────────────────────┬───────────────────────────────────────────────────────────────────────────────┬───────────────────────────────────────────────│
    │write_address     ││ -1                                     │0                                                                              │1                                              │
    │                  ││────────────────────────────────────────┴───────────────────────────────────────────────────────────────────────────────┴───────────────────────────────────────────────│
    │                  ││──────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬───────│
    │dct_coef          ││ 784      │-400     │1448     │2009     │1892     │1703     │1448     │1138     │784      │400      │1448     │1703     │784      │-400     │-1448    │-2009    │-1892  │
    │                  ││──────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴───────│
    │                  ││──────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬───────│
    │dct_mul           ││ -105834  │67424    │-2400    │-2861248 │1711668  │-2260940 │-258856  │-849976  │-2700474 │-2019584 │-314000  │-2861248 │1450956  │-936880  │60800    │849976   │4767357│
    │                  ││──────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴───────│
    │                  ││──────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬───────│
    │dct_mac           ││ 554686   │448852   │516276   │513876   │-2861248 │-1149580 │-3410520 │-3669376 │-4519352 │-7219826 │-9239410 │-9553410 │-2861248 │-1410292 │-2347172 │-2286372 │-143639│
    │                  ││──────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴───────│
    │                  ││──────────┬─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │x                 ││ 7        │0                                                                                                                                                            │
    │                  ││──────────┴─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││──────────┬───────────────────────────────────────────────────────────────────────────────┬─────────────────────────────────────────────────────────────────────────────│
    │y                 ││ 7        │0                                                                              │1                                                                            │
    │                  ││──────────┴───────────────────────────────────────────────────────────────────────────────┴─────────────────────────────────────────────────────────────────────────────│
    │                  ││──────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬───────│
    │z                 ││ 7        │0        │1        │2        │3        │4        │5        │6        │7        │0        │1        │2        │3        │4        │5        │6        │7      │
    │                  ││──────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴───────│
    │pass              ││          ┌─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││──────────┘                                                                                                                                                             │
    └──────────────────┘└────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘ |}]
;;
