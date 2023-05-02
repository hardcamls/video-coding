(** Default quantisation tables. *)

open! Base

(** Default luma and chroma tables as specified in the jpeg standard.  
    Note that these are in zigzag order. *)
val luma : int array

val chroma : int array

(** Scale table by quality factor [1..100]. 
    
    100 has no quantization.  About 75 should give very good quality.
*)
val scale : int array -> int -> int array
