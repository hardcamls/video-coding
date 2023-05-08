(** Basic 8x8 block JPEG data path. 
    
    Huffman decode, Iquant, Idct + level shift.
*)

open Hardcaml

module I : sig
  type 'a t =
    { clocking : 'a Clocking.t
    ; start_codeblock_decoder : 'a
    ; start_idct : 'a
    ; start_output : 'a
    ; dht : 'a Markers.Dht.Fields.t
    ; dqt : 'a Markers.Dqt.Fields.t
    ; ac_table_select : 'a
    ; dc_table_select : 'a
    ; qnt_table_select : 'a
    ; dc_pred_in : 'a
    ; bits : 'a
    ; bits_valid : 'a
    ; pixel_read_address : 'a
    ; pixel_read_enable : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module O : sig
  type 'a t =
    { pixel : 'a
    ; read_bits : 'a
    ; dc_pred_out : 'a
    ; dc_pred_write : 'a
    ; done_ : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

val done_delay
  :  count:int
  -> Signal.t Clocking.t
  -> start:Signal.t
  -> done_:Signal.t
  -> Signal.t

val create : Scope.t -> Interface.Create_fn(I)(O).t
val hierarchical : Scope.t -> Interface.Create_fn(I)(O).t
