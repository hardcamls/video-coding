(** Image and component controller. 
    
    Processes the components and scans of an image as specified, and iterates through 
    the image.

    Controls the decoder datapath.
*)

open! Base
open Hardcaml

val max_components : int
val log_max_components : int
val max_scans : int
val log_max_scans : int

module Ctrl : sig
  type 'a t =
    { codeblock_decoder : 'a
    ; idct : 'a
    ; output : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module Core : sig
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
      ; ac_table_select : 'a
      ; dc_table_select : 'a
      ; qnt_table_select : 'a
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
      ; ac_table_select : 'a
      ; dc_table_select : 'a
      ; qnt_table_select : 'a
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
