open Hardcaml

module I : sig
  type 'a t =
    { clocking : 'a Clocking.t
    ; start : 'a
    ; dht : 'a Markers.Dht.Fields.t
    ; dqt : 'a Markers.Dqt.Fields.t
    ; luma_or_chroma : 'a
    ; dc_pred_in : 'a
    ; bits : 'a
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
