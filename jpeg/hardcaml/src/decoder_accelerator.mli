open Hardcaml

module I : sig
  type 'a t =
    { clocking : 'a Clocking.t
    ; start_codeblock_decoder : 'a
    ; start_idct : 'a
    ; dht : 'a Markers.Dht.Fields.t
    ; dqt : 'a Markers.Dqt.Fields.t
    ; ac_table_select : 'a
    ; dc_table_select : 'a
    ; qnt_table_select : 'a
    ; dc_pred_in : 'a
    ; jpeg : 'a
    ; jpeg_valid : 'a
    ; pixel_read_address : 'a
    ; pixel_read_enable : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module O : sig
  type 'a t =
    { pixel : 'a
    ; dc_pred_out : 'a
    ; jpeg_ready : 'a
    ; done_ : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

val create : Scope.t -> Interface.Create_fn(I)(O).t
