open Base
module Bits = Bitstream_reader.Make (String)

module Component = struct
  type t =
    { identifier : int
    ; horizontal_sampling_factor : int
    ; vertical_sampling_factor : int
    ; quantization_table_identifier : int
    }
  [@@deriving sexp_of]

  let decode bits =
    let identifier = Bits.get bits 8 in
    let vertical_sampling_factor = Bits.get bits 4 in
    let horizontal_sampling_factor = Bits.get bits 4 in
    let quantization_table_identifier = Bits.get bits 8 in
    { identifier
    ; vertical_sampling_factor
    ; horizontal_sampling_factor
    ; quantization_table_identifier
    }
  ;;
end

(* start of frame*)
module Sof = struct
  type t =
    { length : int
    ; sample_precision : int
    ; width : int
    ; height : int
    ; number_of_components : int
    ; components : Component.t array
    }
  [@@deriving sexp_of]

  let decode bits =
    let length = Bits.get bits 16 in
    let sample_precision = Bits.get bits 8 in
    let height = Bits.get bits 16 in
    let width = Bits.get bits 16 in
    let number_of_components = Bits.get bits 8 in
    let components =
      Array.init number_of_components ~f:(fun _ -> Component.decode bits)
    in
    { length; sample_precision; height; width; number_of_components; components }
  ;;
end

module Scan_component = struct
  type t =
    { selector : int
    ; dc_coef_selector : int
    ; ac_coef_selector : int
    }
  [@@deriving sexp_of]

  let decode bits =
    let selector = Bits.get bits 8 in
    let dc_coef_selector = Bits.get bits 4 in
    let ac_coef_selector = Bits.get bits 4 in
    { selector; dc_coef_selector; ac_coef_selector }
  ;;
end

(* start of scan*)
module Sos = struct
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

  let decode bits =
    let length = Bits.get bits 16 in
    let number_of_image_components = Bits.get bits 8 in
    let scan_components =
      Array.init number_of_image_components ~f:(fun _ -> Scan_component.decode bits)
    in
    let start_of_predictor_selection = Bits.get bits 8 in
    let end_of_predictor_selection = Bits.get bits 8 in
    let successive_approximation_bit_high = Bits.get bits 4 in
    let successive_approximation_bit_low = Bits.get bits 4 in
    { length
    ; number_of_image_components
    ; scan_components
    ; start_of_predictor_selection
    ; end_of_predictor_selection
    ; successive_approximation_bit_high
    ; successive_approximation_bit_low
    }
  ;;
end

module Dqt = struct
  type t =
    { length : int
    ; element_precision : int
    ; table_identifier : int
    ; elements : int array
    }
  [@@deriving sexp_of]

  let decode bits =
    let length = Bits.get bits 16 in
    let element_precision = 8 lsl Bits.get bits 4 in
    let table_identifier = Bits.get bits 4 in
    let elements = Array.init 64 ~f:(fun _ -> Bits.get bits element_precision) in
    { length; element_precision; table_identifier; elements }
  ;;
end

module Dri = struct
  type t =
    { length : int
    ; restart_interval : int
    }
  [@@deriving sexp_of]

  let decode bits =
    let length = Bits.get bits 16 in
    let restart_interval = Bits.get bits 16 in
    { length; restart_interval }
  ;;
end

module Dht = struct
  type t =
    { length : int
    ; table_class : int
    ; destination_identifier : int
    ; lengths : int array
    ; values : int array array
    }
  [@@deriving sexp_of]

  let decode bits =
    let length = Bits.get bits 16 in
    let table_class = Bits.get bits 4 in
    let destination_identifier = Bits.get bits 4 in
    let lengths = Array.init 16 ~f:(fun _ -> Bits.get bits 8) in
    let values =
      Array.map lengths ~f:(fun length -> Array.init length ~f:(fun _ -> Bits.get bits 8))
    in
    { length; table_class; destination_identifier; lengths; values }
  ;;
end
