(** Remove stuffing bytes from the entropy coded segment. 
    
    Inputs and outputs upto 16 bits per cycle.
*)

open Hardcaml

module I : sig
  type 'a t =
    { clocking : 'a Clocking.t
    ; i_data : 'a
    ; i_valid : 'a
    ; o_ready : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module O : sig
  type 'a t =
    { o_data : 'a
    ; o_valid : 'a
    ; i_ready : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

val create : Scope.t -> Interface.Create_fn(I)(O).t
val hierarchical : Scope.t -> Interface.Create_fn(I)(O).t
