open Core

let forward_coefs =
  Array.init 8 ~f:(fun j ->
      let j = Float.of_int j in
      Array.init 8 ~f:(fun i ->
          let s = if i = 0 then 1. /. Float.sqrt 2. else 1. in
          let i = Float.of_int i in
          s /. 2. *. Float.cos (Float.pi /. 8. *. i *. (j +. 0.5))))
;;

let transpose8x8 a = Array.init 8 ~f:(fun i -> Array.init 8 ~f:(fun j -> a.(j).(i)))
let inverse_coefs = transpose8x8 forward_coefs

let mul8x8 a b =
  Array.init 8 ~f:(fun row ->
      Array.init 8 ~f:(fun col ->
          let sum = ref 0. in
          for k = 0 to 7 do
            sum := !sum +. (a.(row).(k) *. b.(k).(col))
          done;
          !sum))
;;

let scale8x8 scale m = Array.map m ~f:(Array.map ~f:(fun e -> scale *. e))
let forward_dct a = mul8x8 (mul8x8 forward_coefs a) forward_coefs
let inverse_dct a = mul8x8 (mul8x8 inverse_coefs a) inverse_coefs

let%expect_test "compare" =
  let inputs =
    Array.init 8 ~f:(fun _ -> Array.init 8 ~f:(fun _ -> Random.float 100. -. 50.))
  in
  let chen =
    let frnd x = Float.round_nearest x |> Float.to_int in
    let i = Array.concat (Array.to_list inputs) |> Array.map ~f:frnd in
    Hardcaml_jpeg_model.Dct.forward_dct_8x8 i;
    i
  in
  let reference = forward_dct inputs |> scale8x8 4. in
  print_s [%message (chen : int array) (reference : float array array)];
  [%expect
    {|
    ((chen
      (-145 116 8 -80 -144 131 -15 1 -32 140 -42 226 -92 -15 19 21 -2 120 -114
       -230 -68 -190 -59 194 -17 21 -58 -5 133 -19 -16 -138 111 -52 12 11 7 -146
       -286 36 -132 302 -18 48 67 -41 28 -64 -27 -155 224 -38 25 -10 -2 27 102 82
       -81 -227 23 89 -153 10))
     (reference
      ((-145.58608176591724 131.11220020769238 6.6884204468785953
        34.480218379807859 -159.51250881582268 157.03560117344682
        20.250914004927957 -20.009777714345969)
       (25.277918418936608 101.23669596127807 -58.16413248073566 158.095187809381
        -102.04700176777746 -95.783413767662978 13.595977204282512
        111.45515135958524)
       (-53.611106988992695 128.09606523631831 -121.45251519376046
        -270.48358166382371 -53.329412232339294 -108.50269446777759
        10.344178046676415 136.640133231312)
       (20.828308483368083 -50.204278771169264 -48.748670157608117
        78.050749425615848 102.55307729253236 -7.0683857783951
        -12.272996247083451 -129.30194529222129)
       (113.1476912277471 -20.0011400330298 -57.3817882976355 -50.218946278336368
        -34.889724573878887 -156.18970053706349 -280.94803216394018
        75.997924247568363)
       (-130.61623142074575 322.25013989511643 -76.537923390294324
        125.19009923959433 69.5925052193376 -68.839407891063061
        37.579685721680683 -90.679492435106241)
       (-65.660887669602687 -105.11095668574231 211.84010176924414
        30.086209081634273 15.034792226039535 -90.1394578797693
        -7.3814239457418971 33.780117584747579)
       (67.323685298919941 111.60376943335571 -27.492939861972104
        -207.23995305653449 44.165332826914522 43.355281423753226
        -167.4890947020329 5.36691724673786)))) |}]
;;
