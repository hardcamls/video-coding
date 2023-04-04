(* Tests for decoder code-word magnitudes. *)

let%expect_test "" =
  print_s
    [%message
      "test the magnitude decoding (optimally) from the VLD block.  From there we can \
       finalize the run-length decoder."]
;;
