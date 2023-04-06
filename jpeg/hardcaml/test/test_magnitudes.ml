(* Tests for decoder code-word magnitudes. *)
open Core
open Hardcaml
open Bits

let%expect_test "test upto cat=11" =
  (* I dont think cat=12 is possible - the output range then requires 13 bits which I think is greater
   than the idct input precision - at least with 8 bit images. *)
  for cat = 0 to 11 do
    for code = 0 to (1 lsl cat) - 1 do
      let result =
        Hardcaml_jpeg.Codeblock_decoder.For_testing.decode_magnitude
          (of_int ~width:4 cat)
          (of_int ~width:12 (code lsl (12 - cat)))
        |> Bits.to_sint
      in
      let expected = Hardcaml_jpeg_model.Model.For_testing.mag cat code in
      if result <> expected
      then
        raise_s
          [%message "error" (cat : int) (code : int) (result : int) (expected : int)]
    done
  done
;;
