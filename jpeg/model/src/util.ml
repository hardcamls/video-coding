open Base

let sexp_of_block len (t : int array) =
  let sexp_of_hex x =
    let hex x =
      let x = x land 15 in
      if x < 10
      then Char.of_int_exn (x + Char.to_int '0')
      else Char.of_int_exn (x - 10 + Char.to_int 'a')
    in
    sexp_of_string
      (String.init len ~f:(fun i ->
           let i = len - i - 1 in
           hex (x lsr (i * 4))))
  in
  let block = Array.init 8 ~f:(fun y -> Array.init 8 ~f:(fun x -> t.(x + (y * 8)))) in
  [%sexp_of: hex array array] block
;;

type coef_block = int array

let sexp_of_coef_block = sexp_of_block 3

type pixel_block = int array

let sexp_of_pixel_block = sexp_of_block 2
