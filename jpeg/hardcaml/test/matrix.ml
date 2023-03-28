open Core

type 'a t = 'a array array [@@deriving sexp_of]

let init ~rows ~cols ~f =
  Array.init rows ~f:(fun row -> Array.init cols ~f:(fun col -> f ~row ~col))
;;

let rows t = Array.length t
let cols t = Array.length t.(0)
let get t ~row ~col = t.(row).(col)
let row_vec t ~row = Array.init (cols t) ~f:(fun col -> get t ~row ~col)
let col_vec t ~col = Array.init (rows t) ~f:(fun row -> get t ~row ~col)
let map a ~f = Array.map a ~f:(Array.map ~f)
let map2 a b ~f = Array.map2_exn a b ~f:(Array.map2_exn ~f)

let transpose a =
  init ~rows:(cols a) ~cols:(rows a) ~f:(fun ~row ~col -> get a ~row:col ~col:row)
;;

let add = map2 ~f:( +. )
let sub = map2 ~f:( -. )
let identity n = init ~rows:n ~cols:n ~f:(fun ~row ~col -> if row = col then 1. else 0.)
let scale scl = map ~f:(( *. ) scl)

let mul a b =
  if cols a <> rows b then raise_s [%message "Cannot multiply matrices"];
  init ~rows:(rows a) ~cols:(cols b) ~f:(fun ~row ~col ->
      let sum = ref 0. in
      for i = 0 to cols a - 1 do
        sum := !sum +. (get a ~row ~col:i *. get b ~row:i ~col)
      done;
      !sum)
;;

let foo = init ~rows:3 ~cols:2 ~f:(fun ~row ~col -> row + (col * 10))
let bar = init ~rows:3 ~cols:2 ~f:(fun ~row ~col -> col + (row * 10))
let baz = init ~rows:2 ~cols:4 ~f:(fun ~row ~col -> col + (row * 10))

let%expect_test "init" =
  print_s [%message (foo : int t) (bar : int t) (baz : int t)];
  [%expect
    {|
    ((foo ((0 10) (1 11) (2 12))) (bar ((0 1) (10 11) (20 21)))
     (baz ((0 1 2 3) (10 11 12 13)))) |}]
;;

let%expect_test "transpose" = print_s [%message (transpose bar : int t)];
  [%expect {| ("transpose bar" ((0 10 20) (1 11 21))) |}]

let foo = map foo ~f:Float.of_int
let bar = map bar ~f:Float.of_int
let baz = map baz ~f:Float.of_int

let%expect_test "add" =
  print_s [%message (add foo bar : float t)];
  [%expect {| ("add foo bar" ((0 11) (11 22) (22 33))) |}]
;;

let%expect_test "mul" =
  print_s [%message (mul bar (identity (cols bar)) : float t)];
  [%expect {| ("mul bar (identity (cols bar))" ((0 1) (10 11) (20 21))) |}];
  print_s [%message (mul (identity (rows bar)) bar : float t)];
  [%expect {| ("mul (identity (rows bar)) bar" ((0 1) (10 11) (20 21))) |}];
  print_s [%message (mul foo baz : float t)];
  [%expect
    {|
    ("mul foo baz" ((100 110 120 130) (110 122 134 146) (120 134 148 162))) |}]
;;
