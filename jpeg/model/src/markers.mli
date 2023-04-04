open Base
module Bits = Bitstream_reader.From_string

module Component : sig
  type t =
    { identifier : int
    ; horizontal_sampling_factor : int
    ; vertical_sampling_factor : int
    ; quantization_table_identifier : int
    }
  [@@deriving sexp_of]

  val decode : Bits.t -> t
end

(* start of frame*)
module Sof : sig
  type t =
    { length : int
    ; sample_precision : int
    ; width : int
    ; height : int
    ; number_of_components : int
    ; components : Component.t array
    }
  [@@deriving sexp_of]

  val decode : Bits.t -> t
end

module Scan_component : sig
  type t =
    { selector : int
    ; dc_coef_selector : int
    ; ac_coef_selector : int
    }
  [@@deriving sexp_of]

  val decode : Bits.t -> t
end

(* start of scan*)
module Sos : sig
  type t =
    { length : int
    ; number_of_image_components : int
    ; scan_components : Scan_component.t array
    ; start_of_predictor_selection : int
    ; end_of_predictor_selection : int
    ; successive_approximation_bit_high : int
    ; successive_approximation_bit_low : int
    }
  [@@deriving sexp_of]

  val decode : Bits.t -> t
end

module Dqt : sig
  type t =
    { length : int
    ; element_precision : int
    ; table_identifier : int
    ; elements : int array
    }
  [@@deriving sexp_of]

  val decode : Bits.t -> t
end

module Dri : sig
  type t =
    { length : int
    ; restart_interval : int
    }
  [@@deriving sexp_of]

  val decode : Bits.t -> t
end

module Dht : sig
  type t =
    { length : int
    ; table_class : int
    ; destination_identifier : int
    ; lengths : int array
    ; values : int array array
    }
  [@@deriving sexp_of]

  val decode : Bits.t -> t
end
