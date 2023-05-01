open Base
module Reader = Bitstream_reader.From_string
module Writer = Bitstream_writer

module Component = struct
  type t =
    { identifier : int
    ; horizontal_sampling_factor : int
    ; vertical_sampling_factor : int
    ; quantization_table_identifier : int
    }
  [@@deriving sexp_of]

  let decode bits =
    let identifier = Reader.get bits 8 in
    let vertical_sampling_factor = Reader.get bits 4 in
    let horizontal_sampling_factor = Reader.get bits 4 in
    let quantization_table_identifier = Reader.get bits 8 in
    { identifier
    ; vertical_sampling_factor
    ; horizontal_sampling_factor
    ; quantization_table_identifier
    }
  ;;

  let bytes = 3

  let encode writer t =
    Writer.put_bits writer ~stuffing:false ~bits:8 ~value:t.identifier;
    Writer.put_bits writer ~stuffing:false ~bits:4 ~value:t.horizontal_sampling_factor;
    Writer.put_bits writer ~stuffing:false ~bits:4 ~value:t.vertical_sampling_factor;
    Writer.put_bits writer ~stuffing:false ~bits:8 ~value:t.quantization_table_identifier
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
    let length = Reader.get bits 16 in
    let sample_precision = Reader.get bits 8 in
    let height = Reader.get bits 16 in
    let width = Reader.get bits 16 in
    let number_of_components = Reader.get bits 8 in
    let components =
      Array.init number_of_components ~f:(fun _ -> Component.decode bits)
    in
    { length; sample_precision; height; width; number_of_components; components }
  ;;

  let encode writer t =
    let length = 2 + 6 + (t.number_of_components * Component.bytes) in
    Writer.put_bits writer ~stuffing:false ~bits:16 ~value:length;
    Writer.put_bits writer ~stuffing:false ~bits:8 ~value:t.sample_precision;
    Writer.put_bits writer ~stuffing:false ~bits:16 ~value:t.width;
    Writer.put_bits writer ~stuffing:false ~bits:16 ~value:t.height;
    Writer.put_bits writer ~stuffing:false ~bits:8 ~value:t.number_of_components;
    for i = 0 to t.number_of_components - 1 do
      Component.encode writer t.components.(i)
    done
  ;;
end

module Scan_component = struct
  type t =
    { selector : int
    ; dc_coef_selector : int
    ; ac_coef_selector : int
    }
  [@@deriving sexp_of]

  let bytes = 2

  let decode bits =
    let selector = Reader.get bits 8 in
    let dc_coef_selector = Reader.get bits 4 in
    let ac_coef_selector = Reader.get bits 4 in
    { selector; dc_coef_selector; ac_coef_selector }
  ;;

  let encode writer t =
    Writer.put_bits writer ~stuffing:false ~bits:8 ~value:t.selector;
    Writer.put_bits writer ~stuffing:false ~bits:4 ~value:t.dc_coef_selector;
    Writer.put_bits writer ~stuffing:false ~bits:4 ~value:t.ac_coef_selector
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
    let length = Reader.get bits 16 in
    let number_of_image_components = Reader.get bits 8 in
    let scan_components =
      Array.init number_of_image_components ~f:(fun _ -> Scan_component.decode bits)
    in
    let start_of_predictor_selection = Reader.get bits 8 in
    let end_of_predictor_selection = Reader.get bits 8 in
    let successive_approximation_bit_high = Reader.get bits 4 in
    let successive_approximation_bit_low = Reader.get bits 4 in
    { length
    ; number_of_image_components
    ; scan_components
    ; start_of_predictor_selection
    ; end_of_predictor_selection
    ; successive_approximation_bit_high
    ; successive_approximation_bit_low
    }
  ;;

  let encode writer t =
    let length = 2 + 4 + (t.number_of_image_components * Scan_component.bytes) in
    Writer.put_bits writer ~stuffing:false ~bits:16 ~value:length;
    Writer.put_bits writer ~stuffing:false ~bits:8 ~value:t.number_of_image_components;
    for i = 0 to t.number_of_image_components - 1 do
      Scan_component.encode writer t.scan_components.(i)
    done;
    Writer.put_bits writer ~stuffing:false ~bits:16 ~value:t.start_of_predictor_selection;
    Writer.put_bits writer ~stuffing:false ~bits:16 ~value:t.end_of_predictor_selection;
    Writer.put_bits
      writer
      ~stuffing:false
      ~bits:16
      ~value:t.successive_approximation_bit_high;
    Writer.put_bits
      writer
      ~stuffing:false
      ~bits:16
      ~value:t.successive_approximation_bit_low
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
    let length = Reader.get bits 16 in
    let element_precision = 8 lsl Reader.get bits 4 in
    let table_identifier = Reader.get bits 4 in
    let elements = Array.init 64 ~f:(fun _ -> Reader.get bits element_precision) in
    { length; element_precision; table_identifier; elements }
  ;;

  let encode writer t =
    let element_bytes = t.element_precision / 8 in
    let length = 3 + (64 * element_bytes) in
    Writer.put_bits writer ~stuffing:false ~bits:16 ~value:length;
    Writer.put_bits writer ~stuffing:false ~bits:4 ~value:(element_bytes - 1);
    Writer.put_bits writer ~stuffing:false ~bits:4 ~value:t.table_identifier;
    for i = 0 to 63 do
      Writer.put_bits
        writer
        ~stuffing:false
        ~bits:t.element_precision
        ~value:t.elements.(Zigzag.inverse.(i))
    done
  ;;
end

module Dri = struct
  type t =
    { length : int
    ; restart_interval : int
    }
  [@@deriving sexp_of]

  let decode bits =
    let length = Reader.get bits 16 in
    let restart_interval = Reader.get bits 16 in
    { length; restart_interval }
  ;;
end

module Dht = struct
  type t =
    { length : int
    ; table_class : int
    ; destination_identifier : int
    ; lengths : int array
    ; values : int array
    }
  [@@deriving sexp_of]

  let decode bits =
    let length = Reader.get bits 16 in
    let table_class = Reader.get bits 4 in
    let destination_identifier = Reader.get bits 4 in
    let lengths = Array.init 16 ~f:(fun _ -> Reader.get bits 8) in
    let values =
      let data_length = Array.fold lengths ~init:0 ~f:( + ) in
      Array.init data_length ~f:(fun _ -> Reader.get bits 8)
    in
    { length; table_class; destination_identifier; lengths; values }
  ;;

  let encode writer t =
    let length = 3 + 16 + Array.fold t.lengths ~init:0 ~f:( + ) in
    Writer.put_bits writer ~stuffing:false ~bits:16 ~value:length;
    Writer.put_bits writer ~stuffing:false ~bits:4 ~value:t.table_class;
    Writer.put_bits writer ~stuffing:false ~bits:4 ~value:t.destination_identifier;
    Array.iter t.lengths ~f:(fun length ->
        Writer.put_bits writer ~stuffing:false ~bits:8 ~value:length);
    Array.iter t.values ~f:(fun data ->
        Writer.put_bits writer ~stuffing:false ~bits:8 ~value:data)
  ;;
end
