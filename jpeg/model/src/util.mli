(** Utilities*)

(** {2 Functions for sexp pretty printing blocks of pixels and coefs in hex}. *)

val sexp_of_block : int -> int array -> Sexplib0.Sexp.t

type coef_block = int array

val sexp_of_coef_block : int array -> Sexplib0.Sexp.t

type pixel_block = int array

val sexp_of_pixel_block : int array -> Sexplib0.Sexp.t
