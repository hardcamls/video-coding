open Core

include struct
  open Hardcaml_video_common
  module Frame = Frame
  module Plane = Plane
  module Reader = Bitstream_reader
end

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

let mouse480 = "../../test_data/Mouse480.jpg"
let load_jpeg_file filename = In_channel.read_all filename |> Reader.From_string.create

let headers_and_entropy_coded_segment filename =
  let bits = In_channel.with_file ~binary:true filename ~f:In_channel.input_all in
  let reader = Reader.From_string.create bits in
  let header = Decoder.Header.decode reader in
  let entropy_bits = Decoder.For_testing.extract_entropy_coded_bits reader in
  header, Reader.From_string.get_buffer entropy_bits
;;

let remove_stuffing_bytes bits =
  let buffer = Buffer.create 1024 in
  let rec search_for_marker pos prev =
    if pos = String.length bits
    then Buffer.contents buffer
    else (
      let c = bits.[pos] in
      if Char.equal prev '\xff'
      then
        if Char.equal c '\x00'
        then (
          Buffer.add_char buffer prev;
          search_for_marker (pos + 1) c)
        else Buffer.contents buffer
      else if Char.equal c '\xff'
      then search_for_marker (pos + 1) c
      else (
        Buffer.add_char buffer c;
        search_for_marker (pos + 1) c))
  in
  search_for_marker 0 '\x00'
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
    let bits = String.to_list bits |> List.map ~f:of_char |> concat_msb in
    let bits =
      reg_fb
        (Reg_spec.override (Clocking.to_spec clocking) ~clear_to:bits)
        ~width:(width bits)
        ~f:(fun d -> log_shift sll d read_bits)
    in
    { O.bits = bits.:-[None, 16] }
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
    let read_bits = wire 1 -- "READ_BITS" in
    let bits =
      Super_simple_bitstream_reader.hierarchical
        ~bits
        scope
        { Super_simple_bitstream_reader.I.clocking = i.clocking
        ; read_bits = mux2 read_bits (of_int ~width:5 8) (zero 5)
        }
    in
    let decoder =
      D.hierarchical
        scope
        { D.I.clocking = i.clocking
        ; start = i.start
        ; bits = bits.bits.:[15, 8] -- "BITS"
        ; bits_valid = vdd
        }
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

let ( <--. ) a b = a := Bits.of_int ~width:(Bits.width !a) b

let load_huffman_tables
    ~cycle
    (dht : Bits.t ref Hardcaml_jpeg.Markers.Dht.Fields.t)
    (huffman_tables : Hardcaml_jpeg_model.Markers.Dht.t list)
  =
  List.iter huffman_tables ~f:(fun t ->
      let lengths = t.lengths in
      dht.header.destination_identifier <--. t.destination_identifier;
      dht.header.table_class <--. t.table_class;
      let pos = ref 0 in
      let code = ref 0 in
      dht.code.code_write := Bits.vdd;
      for i = 0 to Array.length lengths - 1 do
        dht.code.code_base_address <--. !pos;
        dht.code.code_length_minus1 <--. i;
        dht.code.num_codes_at_length <--. lengths.(i);
        dht.code.code <--. !code;
        code := (!code + lengths.(i)) lsl 1;
        pos := !pos + lengths.(i);
        cycle ()
      done;
      dht.code.code_write := Bits.gnd;
      dht.code_data.data_write := Bits.vdd;
      for i = 0 to Array.length t.values - 1 do
        dht.code_data.data_address <--. i;
        dht.code_data.data <--. t.values.(i);
        cycle ()
      done;
      dht.code_data.data_write := Bits.gnd)
;;

let load_quant_tables
    ~cycle
    (dqt : Bits.t ref Hardcaml_jpeg.Markers.Dqt.Fields.t)
    (quant_tables : Hardcaml_jpeg_model.Markers.Dqt.t list)
  =
  List.iter quant_tables ~f:(fun t ->
      dqt.fields.table_identifier <--. t.table_identifier;
      dqt.element_write := Bits.vdd;
      for i = 0 to Array.length t.elements - 1 do
        dqt.element_address <--. i;
        dqt.element <--. t.elements.(i);
        cycle ()
      done;
      dqt.element_write := Bits.gnd)
;;

let reconstruct ~block_number frame pixels =
  let macroblock = block_number / 6 in
  let subblock = block_number % 6 in
  let width = Frame.width frame in
  let y_pos = macroblock / (width / 16) in
  let x_pos = macroblock % (width / 16) in
  let copy plane x_pos y_pos =
    for y = 0 to 7 do
      for x = 0 to 7 do
        Plane.(plane.![x_pos + x, y_pos + y] <- Char.of_int_exn pixels.(x + (y * 8)))
      done
    done
  in
  match subblock with
  | 0 -> copy (Frame.y frame) (x_pos * 16) (y_pos * 16)
  | 1 -> copy (Frame.y frame) ((x_pos * 16) + 8) (y_pos * 16)
  | 2 -> copy (Frame.y frame) (x_pos * 16) ((y_pos * 16) + 8)
  | 3 -> copy (Frame.y frame) ((x_pos * 16) + 8) ((y_pos * 16) + 8)
  | 4 -> copy (Frame.u frame) (x_pos * 8) (y_pos * 8)
  | 5 -> copy (Frame.v frame) (x_pos * 8) (y_pos * 8)
  | _ -> failwith ""
;;
