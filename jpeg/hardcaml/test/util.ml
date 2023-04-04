open Core

let marker_length bits ~pos =
  List.init 2 ~f:(fun i -> Char.to_int bits.[pos + i])
  |> List.fold ~init:0 ~f:(fun acc a -> (acc lsl 8) lor a)
;;

let find_next_marker ~start_pos ~marker_code bits =
  let marker = Char.of_int_exn marker_code in
  With_return.with_return_option (fun r ->
      for pos = start_pos to String.length bits - 2 do
        if Char.equal bits.[pos] '\xff' && Char.equal bits.[pos + 1] marker
        then r.return pos
      done)
;;

let find_next_marker_exn ~start_pos ~marker_code bits =
  find_next_marker ~start_pos ~marker_code bits |> Option.value_exn
;;

let extract_next_marker ~start_pos ~marker_code bits =
  let pos = find_next_marker ~start_pos ~marker_code bits in
  Option.map pos ~f:(fun pos ->
      let pos = pos + 2 in
      let len = marker_length bits ~pos in
      String.sub ~pos ~len bits)
;;

let extract_next_marker_exn ~start_pos ~marker_code bits =
  extract_next_marker ~start_pos ~marker_code bits |> Option.value_exn
;;

let find_nth_marker_exn ~n ~marker_code bits =
  let rec find n start_pos =
    let start_pos = find_next_marker_exn ~start_pos ~marker_code bits in
    if n = 0
    then extract_next_marker_exn ~start_pos ~marker_code bits
    else find (n - 1) (start_pos + 1)
  in
  find n 0
;;

open Hardcaml_jpeg_model

let headers_and_entropy_coded_segment filename =
  let bits = In_channel.with_file ~binary:true filename ~f:In_channel.input_all in
  let reader = Bitstream_reader.From_string.create bits in
  let header = Model.Header.decode reader Model.Header.empty in
  let entropy_bits = Model.extract_entropy_coded_bits reader in
  header, Bitstream_reader.From_string.get_buffer entropy_bits
;;

open Hardcaml
open Signal
open Hardcaml_jpeg

(* Can we do a super simple bitstream read for simulation where we preload the data and just pull it out?  I think so. *)
module Super_simple_bitstream_reader = struct
  module I = struct
    type 'a t =
      { clocking : 'a Clocking.t
      ; read_bits : 'a [@bits 5]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { bits : 'a [@bits 16] } [@@deriving sexp_of, hardcaml]
  end

  let create ~bits _scope { I.clocking; read_bits } =
    let bits = String.to_list bits |> List.map ~f:of_char |> concat_lsb in
    let bits =
      reg_fb
        (Reg_spec.override (Clocking.to_spec clocking) ~clear_to:bits)
        ~width:(width bits)
        ~f:(fun d -> log_shift srl d read_bits)
    in
    { O.bits = bits.:[15, 0] }
  ;;

  let hierarchical ~bits scope =
    let module Hier = Hierarchy.In_scope (I) (O) in
    Hier.hierarchical ~scope ~name:"ssbsr" (create ~bits)
  ;;
end

module Wrapped_marker_decoder (Fields : Interface.S) (D : Fields_decoder.M(Fields).S) =
struct
  module I = struct
    type 'a t =
      { clocking : 'a Clocking.t
      ; start : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { fields : 'a Fields.t
      ; done_ : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  let create ~bits scope (i : _ I.t) =
    let ( -- ) = Scope.naming scope in
    let read_bits = wire 5 -- "READ_BITS" in
    let bits =
      Super_simple_bitstream_reader.hierarchical
        ~bits
        scope
        { Super_simple_bitstream_reader.I.clocking = i.clocking; read_bits }
    in
    let decoder =
      D.hierarchical
        scope
        { D.I.clocking = i.clocking; start = i.start; bits = bits.bits -- "BITS" }
    in
    read_bits <== decoder.read_bits;
    { O.fields = decoder.fields; done_ = decoder.done_ }
  ;;

  module Sim = Cyclesim.With_interface (I) (O)

  let test ?(waves = true) ?(on_cycle = fun _ -> ()) bits =
    let sim =
      Sim.create
        ~config:Cyclesim.Config.trace_all
        (create ~bits (Scope.create ~flatten_design:true ()))
    in
    let waves, sim =
      if waves
      then (
        let waves, sim = Hardcaml_waveterm.Waveform.create sim in
        Some waves, sim)
      else None, sim
    in
    let inputs = Cyclesim.inputs sim in
    let outputs = Cyclesim.outputs sim in
    let cycle () =
      Cyclesim.cycle sim;
      on_cycle outputs
    in
    inputs.clocking.clear := Bits.vdd;
    cycle ();
    inputs.clocking.clear := Bits.gnd;
    inputs.start := Bits.vdd;
    cycle ();
    inputs.start := Bits.gnd;
    cycle ();
    while not (Bits.to_bool !(outputs.done_)) do
      cycle ()
    done;
    cycle ();
    waves
  ;;
end
