open! Core
open Hardcaml
open Hardcaml_jpeg
open Hardcaml_waveterm

module Test (X : sig
  module Fields : Interface.S

  val marker_code : int
  val second_marker : bool
end) =
struct
  open X
  module Decoder = Fields_decoder.Make (Fields)

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

  open Signal

  let create ~bits scope (i : _ I.t) =
    let read = wire 1 in
    let reader =
      Util.Super_simple_bitstream_reader.(
        create
          ~bits
          scope
          { I.clocking = i.clocking; read_bits = mux2 read (of_int ~width:5 8) (zero 5) })
    in
    let decoder =
      Decoder.(
        create
          scope
          { I.clocking = i.clocking
          ; start = i.start
          ; bits = reader.bits.:[15, 8]
          ; bits_valid = vdd
          })
    in
    read <== decoder.read_bits;
    { O.fields = decoder.fields; done_ = decoder.done_ }
  ;;

  module Sim = Cyclesim.With_interface (I) (O)

  let run () =
    let jpeg = Util.load_jpeg_file Util.mouse480 in
    let bits =
      if second_marker
      then (
        let pos =
          Util.find_next_marker_exn
            ~start_pos:0
            ~marker_code
            (Hardcaml_jpeg_model.Bitstream_reader.From_string.get_buffer jpeg)
        in
        Util.extract_next_marker_exn
          ~start_pos:(pos + 2)
          ~marker_code
          (Hardcaml_jpeg_model.Bitstream_reader.From_string.get_buffer jpeg))
      else
        Util.extract_next_marker_exn
          ~start_pos:0
          ~marker_code
          (Hardcaml_jpeg_model.Bitstream_reader.From_string.get_buffer jpeg)
    in
    print_s [%message (bits : String.Hexdump.t)];
    let sim = Sim.create (create ~bits (Scope.create ())) in
    let waves, sim = Waveform.create sim in
    let inputs = Cyclesim.inputs sim in
    inputs.clocking.clear := Bits.vdd;
    Cyclesim.cycle sim;
    inputs.clocking.clear := Bits.gnd;
    inputs.start := Bits.vdd;
    Cyclesim.cycle sim;
    inputs.start := Bits.gnd;
    for _ = 0 to 15 do
      Cyclesim.cycle sim
    done;
    Waveform.print waves ~display_height:30 ~display_width:110 ~wave_width:2
  ;;
end

let%expect_test "sof" =
  let module T =
    Test (struct
      module Fields = Hardcaml_jpeg.Markers.Sof.Header.Fields

      let marker_code = Hardcaml_jpeg_model.Marker_code.sof0
      let second_marker = false
    end)
  in
  T.run ();
  [%expect
    {|
    (bits
     ("00000000  00 11 08 01 40 01 e0 03  01 22 00 02 11 01 03 11  |....@....\"......|"
      "00000010  01                                                |.|"))
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────────────────────────┐
    │clock             ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐│
    │                  ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └│
    │clear             ││──────┐                                                                                 │
    │                  ││      └─────────────────────────────────────────────────────────────────────────────────│
    │start             ││      ┌─────┐                                                                           │
    │                  ││──────┘     └───────────────────────────────────────────────────────────────────────────│
    │done_             ││────────────┐                                               ┌───────────────────────────│
    │                  ││            └───────────────────────────────────────────────┘                           │
    │                  ││──────────────────────────────────────────┬─────┬─────┬─────┬───────────────────────────│
    │height            ││ 0000                                     │0011 │1108 │0801 │0140                       │
    │                  ││──────────────────────────────────────────┴─────┴─────┴─────┴───────────────────────────│
    │                  ││────────────────────────────────────────────────────────────┬───────────────────────────│
    │length            ││ 0000                                                       │0011                       │
    │                  ││────────────────────────────────────────────────────────────┴───────────────────────────│
    │                  ││────────────────────────┬─────┬─────┬─────┬─────┬─────┬─────┬───────────────────────────│
    │number_of_componen││ 00                     │11   │08   │01   │40   │01   │E0   │03                         │
    │                  ││────────────────────────┴─────┴─────┴─────┴─────┴─────┴─────┴───────────────────────────│
    │                  ││──────────────────────────────────────────────────────┬─────┬───────────────────────────│
    │sample_precision  ││ 00                                                   │11   │08                         │
    │                  ││──────────────────────────────────────────────────────┴─────┴───────────────────────────│
    │                  ││──────────────────────────────┬─────┬─────┬─────┬─────┬─────┬───────────────────────────│
    │width             ││ 0000                         │0011 │1108 │0801 │0140 │4001 │01E0                       │
    │                  ││──────────────────────────────┴─────┴─────┴─────┴─────┴─────┴───────────────────────────│
    │                  ││                                                                                        │
    │                  ││                                                                                        │
    │                  ││                                                                                        │
    │                  ││                                                                                        │
    │                  ││                                                                                        │
    └──────────────────┘└────────────────────────────────────────────────────────────────────────────────────────┘ |}]
;;

let%expect_test "sos header" =
  let module T =
    Test (struct
      module Fields = Hardcaml_jpeg.Markers.Sos.Header.Fields

      let marker_code = Hardcaml_jpeg_model.Marker_code.sos
      let second_marker = false
    end)
  in
  T.run ();
  [%expect
    {|
    (bits
     ("00000000  00 0c 03 01 00 02 11 03  11 00 3f 00              |..........?.|"))
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────────────────────────┐
    │clock             ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐│
    │                  ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └│
    │clear             ││──────┐                                                                                 │
    │                  ││      └─────────────────────────────────────────────────────────────────────────────────│
    │start             ││      ┌─────┐                                                                           │
    │                  ││──────┘     └───────────────────────────────────────────────────────────────────────────│
    │done_             ││────────────┐                 ┌─────────────────────────────────────────────────────────│
    │                  ││            └─────────────────┘                                                         │
    │                  ││──────────────────────────────┬─────────────────────────────────────────────────────────│
    │length            ││ 0000                         │000C                                                     │
    │                  ││──────────────────────────────┴─────────────────────────────────────────────────────────│
    │                  ││────────────────────────┬─────┬─────────────────────────────────────────────────────────│
    │number_of_image_co││ 00                     │0C   │03                                                       │
    │                  ││────────────────────────┴─────┴─────────────────────────────────────────────────────────│
    │                  ││                                                                                        │
    │                  ││                                                                                        │
    │                  ││                                                                                        │
    │                  ││                                                                                        │
    │                  ││                                                                                        │
    │                  ││                                                                                        │
    │                  ││                                                                                        │
    │                  ││                                                                                        │
    │                  ││                                                                                        │
    │                  ││                                                                                        │
    │                  ││                                                                                        │
    │                  ││                                                                                        │
    │                  ││                                                                                        │
    │                  ││                                                                                        │
    └──────────────────┘└────────────────────────────────────────────────────────────────────────────────────────┘ |}]
;;

let%expect_test "dqt header" =
  let module T =
    Test (struct
      module Fields = Hardcaml_jpeg.Markers.Dqt.Header.Fields

      let marker_code = Hardcaml_jpeg_model.Marker_code.dqt
      let second_marker = true
    end)
  in
  T.run ();
  [%expect
    {|
    (bits
     ("00000000  00 43 01 2b 2d 2d 3c 35  3c 76 41 41 76 f8 a5 8c  |.C.+--<5<vAAv...|"
      "00000010  a5 f8 f8 f8 f8 f8 f8 f8  f8 f8 f8 f8 f8 f8 f8 f8  |................|"
      "00000020  f8 f8 f8 f8 f8 f8 f8 f8  f8 f8 f8 f8 f8 f8 f8 f8  |................|"
      "00000030  f8 f8 f8 f8 f8 f8 f8 f8  f8 f8 f8 f8 f8 f8 f8 f8  |................|"
      "00000040  f8 f8 f8                                          |...|"))
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────────────────────────┐
    │clock             ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐│
    │                  ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └│
    │clear             ││──────┐                                                                                 │
    │                  ││      └─────────────────────────────────────────────────────────────────────────────────│
    │start             ││      ┌─────┐                                                                           │
    │                  ││──────┘     └───────────────────────────────────────────────────────────────────────────│
    │done_             ││────────────┐                 ┌─────────────────────────────────────────────────────────│
    │                  ││            └─────────────────┘                                                         │
    │                  ││────────────────────────┬─────┬─────────────────────────────────────────────────────────│
    │element_precision ││ 0                      │4    │0                                                        │
    │                  ││────────────────────────┴─────┴─────────────────────────────────────────────────────────│
    │                  ││──────────────────────────────┬─────────────────────────────────────────────────────────│
    │length            ││ 0000                         │0043                                                     │
    │                  ││──────────────────────────────┴─────────────────────────────────────────────────────────│
    │                  ││────────────────────────┬─────┬─────────────────────────────────────────────────────────│
    │table_identifier  ││ 0                      │3    │1                                                        │
    │                  ││────────────────────────┴─────┴─────────────────────────────────────────────────────────│
    │                  ││                                                                                        │
    │                  ││                                                                                        │
    │                  ││                                                                                        │
    │                  ││                                                                                        │
    │                  ││                                                                                        │
    │                  ││                                                                                        │
    │                  ││                                                                                        │
    │                  ││                                                                                        │
    │                  ││                                                                                        │
    │                  ││                                                                                        │
    │                  ││                                                                                        │
    └──────────────────┘└────────────────────────────────────────────────────────────────────────────────────────┘ |}]
;;
