(* Test forward and inverse Chen DCTs against a reference float DCT *)
open Core

let forward () =
  let input = Array.init 64 ~f:(fun _ -> Random.int 256 - 128) in
  let fdct = Array.copy input in
  Hardcaml_jpeg_model.Dct.Chen.forward_8x8 fdct;
  input, fdct
;;

let inverse fdct =
  let idct = Array.copy fdct in
  Hardcaml_jpeg_model.Dct.Chen.inverse_8x8 idct;
  idct
;;

let forward_and_inverse_transform () =
  let input, fdct = forward () in
  let idct = inverse fdct in
  input, fdct, idct
;;

let check_accuracy ~input ~idct =
  Array.iter2_exn input idct ~f:(fun a b ->
      let b = Float.of_int b /. 4. in
      let b = Float.round_nearest b |> Float.to_int in
      (* a tolerance of 2 seems to work. Thats fine - the accuracy of the IDCT
         is more important, and I think is better than this. *)
      if Int.abs (a - b) > 2
      then raise_s [%message (a : int) (b : int) (input : int array) (idct : int array)])
;;

let test_chen_transform () =
  let input, _, idct = forward_and_inverse_transform () in
  check_accuracy ~input ~idct
;;

let dump t =
  for row = 0 to 7 do
    for col = 0 to 7 do
      printf "%4i " t.((row * 8) + col)
    done;
    printf "\n"
  done
;;

let%expect_test "example transform" =
  let input, fdct = forward () in
  printf "input\n";
  dump input;
  printf "fdct\n";
  (* the forward transform is scaled by 4. *)
  let fdct = Array.map fdct ~f:(fun x -> if x > 0 then (x + 2) / 4 else (x - 2) / 4) in
  dump fdct;
  printf "idct\n";
  let idct = inverse fdct in
  dump idct;
  [%expect
    {|
    input
      62  -84  -28   34  -25   71    6  -29
      85 -128  -92 -105   88 -120  -17   23
     111   89   20 -105  104   27  -43  -49
      65 -127   -7   43  -64   47  127  -71
      10  -78   95  -77   42    3   32   15
      12  -72  -93   80  -76  113  -81  -75
    -100  106  -38   81  -76  -65   24   -2
     -94  102  -25   45    4   21   38  -50
    fdct
     -35    1  -16  113   10   77    0    2
      -4    8   81  135  127   24  110  166
     -22    2  -14  -50   15  -67 -107  -85
     -56  -51  -32  -13   44   81   99  -96
      59  -69  -37  -14  -81  112   40   25
      39   21  -15  -27  -74   98  -43 -137
      48   73  -61  108  -48  -49   72 -101
      74   58   23  -73  -61 -152  -12   91
    idct
      61  -85  -29   33  -26   71    5  -30
      84 -128  -93 -105   88 -121  -18   23
     110   89   20 -106  105   27  -44  -50
      65 -128   -7   43  -63   47  128  -72
      10  -78   96  -77   42    3   32   15
      12  -72  -93   80  -76  113  -81  -76
     -99  107  -37   81  -76  -65   25   -3
     -94  102  -25   45    4   21   38  -50 |}]
;;

let%expect_test "compare accuracy forward and inverse dcts" =
  for _ = 1 to 100 do
    test_chen_transform ()
  done;
  [%expect {||}]
;;
