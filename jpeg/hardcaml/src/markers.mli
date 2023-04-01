open! Base
open! Hardcaml

module Component : sig
  module Fields : sig
    type 'a t =
      { identifier : 'a
      ; vertical_sampling_factor : 'a
      ; horizontal_sampling_factor : 'a
      ; quantization_table_identifier : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end
end

module Sof : sig
  module Header : sig
    module Fields : sig
      type 'a t =
        { length : 'a
        ; sample_precision : 'a
        ; height : 'a
        ; width : 'a
        ; number_of_components : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end
  end

  module Fields : sig
    type 'a t =
      { frame_header : 'a Header.Fields.t
      ; component : 'a Component.Fields.t
      ; component_address : 'a
      ; component_write : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  include Fields_decoder.M(Fields).S
end

module Dqt : sig
  module Header : sig
    module Fields : sig
      type 'a t =
        { length : 'a
        ; element_precision : 'a
        ; table_identifier : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end
  end

  module Fields : sig
    type 'a t =
      { fields : 'a Header.Fields.t
      ; element : 'a
      ; element_address : 'a [@bits 6]
      ; element_write : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  include Fields_decoder.M(Fields).S
end

module Dht : sig
  module Header : sig
    module Fields : sig
      type 'a t =
        { length : 'a
        ; table_class : 'a
        ; destination_identifier : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end
  end

  module Code : sig
    type 'a t =
      { code_length_minus1 : 'a
      ; num_codes_at_length : 'a
      ; code : 'a
      ; code_base_address : 'a
      ; code_write : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module Fields : sig
    type 'a t =
      { header : 'a Header.Fields.t
      ; data : 'a
      ; data_address : 'a
      ; data_write : 'a
      ; code : 'a Code.t
      }
    [@@deriving sexp_of, hardcaml]
  end

  include Fields_decoder.M(Fields).S
end
