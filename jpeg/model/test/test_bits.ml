open! Core

include struct
  open Hardcaml_jpeg_model
  module Reader = Bitstream_reader.From_string
  module Writer = Bitstream_writer
end

let test values =
  let writer = Writer.create () in
  List.iter values ~f:(fun (bits, value) -> Writer.put_bits writer ~bits ~value);
  Writer.flush_with_1s writer;
  let buffer = Writer.get_buffer writer in
  let reader = Reader.create buffer in
  List.iter values ~f:(fun (bits, expected_value) ->
      let pos = Reader.bit_pos reader in
      let value = Reader.get reader bits in
      if value <> expected_value
      then
        raise_s [%message (pos : int) (bits : int) (value : int) (expected_value : int)])
;;

let%expect_test "simple single bits" =
  test [ 1, 0; 1, 1; 1, 1; 1, 0; 1, 0 ];
  [%expect {| |}]
;;

let%expect_test "write 10_000 random bit values, then read and check them all" =
  let values =
    List.init 10_000 ~f:(fun _ ->
        let bits = Random.int 16 + 1 in
        let value = Random.int (1 lsl bits) in
        bits, value)
  in
  test values;
  [%expect {| |}]
;;
