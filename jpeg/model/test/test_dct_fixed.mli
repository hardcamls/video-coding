open Base

type eval_result =
  { unscaled : int
  ; scaled_and_clipped : int
  ; floating : float
  }
[@@deriving sexp_of]

val scale_and_clip : int -> fixed_prec:int -> int

val eval_in_fixed_point
  :  coefs:float array
  -> inputs:int array
  -> fixed_prec:int
  -> eval_result
