open! Base
open Hardcaml

module Ctrl : sig
  type 'a t =
    { codeblock_decoder : 'a
    ; idct : 'a
    ; output : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module I : sig
  type 'a t =
    { clocking : 'a Clocking.t
    ; start : 'a
    ; done_ : 'a Ctrl.t
    ; dc_pred_in : 'a
    ; dc_pred_write : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module O : sig
  type 'a t =
    { start : 'a Ctrl.t
    ; done_ : 'a
    ; dc_pred_out : 'a
    ; luma_or_chroma : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

val create : Scope.t -> Interface.Create_fn(I)(O).t
val hierarchical : Scope.t -> Interface.Create_fn(I)(O).t

module New : sig
  module I : sig
    type 'a t =
      { clocking : 'a Clocking.t
      ; start : 'a
      ; sof : 'a Markers.Sof.Fields.t
      ; sos : 'a Markers.Sos.Fields.t
      ; dc_pred_in : 'a
      ; dc_pred_write : 'a
      ; all_done : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { done_ : 'a
      ; dc_pred_out : 'a
      ; luma_or_chroma : 'a
      ; x_pos : 'a
      ; y_pos : 'a
      ; scan_index : 'a
      ; starter : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module State : sig
    type t

    val strings : string list
  end

  val create : Scope.t -> Interface.Create_fn(I)(O).t
  val hierarchical : Scope.t -> Interface.Create_fn(I)(O).t
end

module With_pipeline_coontrol : sig
  module I : sig
    type 'a t =
      { clocking : 'a Clocking.t
      ; start : 'a
      ; sof : 'a Markers.Sof.Fields.t
      ; sos : 'a Markers.Sos.Fields.t
      ; dc_pred_in : 'a [@bits 12]
      ; dc_pred_write : 'a
      ; dones : 'a Ctrl.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { done_ : 'a
      ; dc_pred_out : 'a
      ; luma_or_chroma : 'a
      ; x_pos : 'a
      ; y_pos : 'a
      ; scan_index : 'a
      ; starts : 'a Ctrl.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  val create : Scope.t -> Interface.Create_fn(I)(O).t
  val hierarchical : Scope.t -> Interface.Create_fn(I)(O).t
end
