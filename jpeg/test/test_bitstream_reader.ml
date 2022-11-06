open Core
open Hardcaml
open Hardcaml_waveterm
module Clocking = Hardcaml_jpeg.Clocking
module Reader = Hardcaml_jpeg.Bitstream_reader
module Sim = Cyclesim.With_interface (Reader.I) (Reader.O)

let create_sim () =
  let sim = Sim.create ~config:Cyclesim.Config.trace_all Reader.create in
  let waves, sim = Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  sim, waves, inputs, outputs
;;

let test_bits_per_cycle bits_per_cycle =
  let sim, waves, inputs, outputs = create_sim () in
  let counter = ref 1 in
  let cycle () =
    Cyclesim.cycle sim;
    if Bits.to_int !(outputs.read_bits_in) <> 0
    then (
      inputs.bits_in := Bits.of_int ~width:16 (0x0101 * !counter);
      Int.incr counter)
  in
  inputs.clocking.clear := Bits.vdd;
  cycle ();
  inputs.clocking.clear := Bits.gnd;
  inputs.bits_in_available := Bits.vdd;
  cycle ();
  cycle ();
  inputs.advance_bits := Bits.of_int ~width:5 bits_per_cycle;
  cycle ();
  for _ = 0 to 3 do
    cycle ()
  done;
  waves
;;

let display_rules =
  let hex = Wave_format.Bit_or Hex in
  let uint = Wave_format.Bit_or Unsigned_int in
  let inputs =
    { Reader.I.(map t ~f:(Fn.const hex)) with
      advance_bits = uint
    ; bits_in_available = uint
    }
  in
  let outputs =
    { Reader.O.(map t ~f:(Fn.const hex)) with
      bits_out_available = uint
    ; read_bits_in = uint
    }
  in
  Display_rule.(
      (List.concat
         [ Reader.I.(
             to_list
             @@ map2 port_names inputs ~f:(fun name wave_format ->
                    port_name_is name ~wave_format))
         ; Reader.O.(
             to_list
             @@ map2 port_names outputs ~f:(fun name wave_format ->
                    port_name_is name ~wave_format))
         ; [ port_name_is
               "num_bits_available"
               ~wave_format:Unsigned_int
           ; port_name_is "bits_buffer" ~wave_format:Hex
           ; port_name_is
               "bits_buffer_offset"
               ~wave_format:Unsigned_int
           ]
         ]))
;;

let%expect_test "Simple bits/cycle" =
  let waves = test_bits_per_cycle 4 in
  Waveform.print ~display_height:30 ~display_width:90 ~wave_width:3 ~display_rules waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────┐
    │clock             ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───│
    │                  ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   │
    │clear             ││────────┐                                                           │
    │                  ││        └───────────────────────────────────────────────────────    │
    │                  ││────────────────────────┬───────────────────────────────────────    │
    │advance_bits      ││ 0                      │4                                          │
    │                  ││────────────────────────┴───────────────────────────────────────    │
    │                  ││────────┬───────┬───────┬───────────────────────────────┬───────    │
    │bits_in           ││ 0000   │0101   │0202   │0303                           │0404       │
    │                  ││────────┴───────┴───────┴───────────────────────────────┴───────    │
    │bits_in_available ││        ┌───────────────────────────────────────────────────────    │
    │                  ││────────┘                                                           │
    │                  ││────────────────┬───────────────┬───────┬───────┬───────┬───────    │
    │bits              ││ 0000           │0101           │2010   │0201   │2020   │0202       │
    │                  ││────────────────┴───────────────┴───────┴───────┴───────┴───────    │
    │bits_out_available││                ┌───────────────────────────────────────────────    │
    │                  ││────────────────┘                                                   │
    │read_bits_in      ││────────────────────────────────┐                       ┌───────    │
    │                  ││                                └───────────────────────┘           │
    │                  ││────────────────┬───────┬───────┬───────┬───────┬───────┬───────    │
    │num_bits_available││ 0              │16     │32     │44     │40     │36     │32         │
    │                  ││────────────────┴───────┴───────┴───────┴───────┴───────┴───────    │
    │                  ││────────────────┬───────┬───────┬───────────────────────────────    │
    │bits_buffer       ││ 000000000000   │010100.│020201.│030302020101                       │
    │                  ││────────────────┴───────┴───────┴───────────────────────────────    │
    │                  ││────────────────┬───────┬───────┬───────┬───────┬───────┬───────    │
    │bits_buffer_offset││ 48             │32     │16     │4      │8      │12     │16         │
    │                  ││────────────────┴───────┴───────┴───────┴───────┴───────┴───────    │
    └──────────────────┘└────────────────────────────────────────────────────────────────────┘ |}];
  let waves = test_bits_per_cycle 8 in
  Waveform.print ~display_height:30 ~display_width:90 ~wave_width:3 ~display_rules waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────┐
    │clock             ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───│
    │                  ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   │
    │clear             ││────────┐                                                           │
    │                  ││        └───────────────────────────────────────────────────────    │
    │                  ││────────────────────────┬───────────────────────────────────────    │
    │advance_bits      ││ 0                      │8                                          │
    │                  ││────────────────────────┴───────────────────────────────────────    │
    │                  ││────────┬───────┬───────┬───────────────┬───────────────┬───────    │
    │bits_in           ││ 0000   │0101   │0202   │0303           │0404           │0505       │
    │                  ││────────┴───────┴───────┴───────────────┴───────────────┴───────    │
    │bits_in_available ││        ┌───────────────────────────────────────────────────────    │
    │                  ││────────┘                                                           │
    │                  ││────────────────┬───────────────┬───────┬───────┬───────┬───────    │
    │bits              ││ 0000           │0101           │0201   │0202   │0302   │0303       │
    │                  ││────────────────┴───────────────┴───────┴───────┴───────┴───────    │
    │bits_out_available││                ┌───────────────────────────────────────────────    │
    │                  ││────────────────┘                                                   │
    │read_bits_in      ││────────────────────────────────┐       ┌───────┐       ┌───────    │
    │                  ││                                └───────┘       └───────┘           │
    │                  ││────────────────┬───────┬───────┬───────┬───────┬───────┬───────    │
    │num_bits_available││ 0              │16     │32     │40     │32     │40     │32         │
    │                  ││────────────────┴───────┴───────┴───────┴───────┴───────┴───────    │
    │                  ││────────────────┬───────┬───────┬───────────────┬───────────────    │
    │bits_buffer       ││ 000000000000   │010100.│020201.│030302020101   │040403030202       │
    │                  ││────────────────┴───────┴───────┴───────────────┴───────────────    │
    │                  ││────────────────┬───────┬───────┬───────┬───────┬───────┬───────    │
    │bits_buffer_offset││ 48             │32     │16     │8      │16     │8      │16         │
    │                  ││────────────────┴───────┴───────┴───────┴───────┴───────┴───────    │
    └──────────────────┘└────────────────────────────────────────────────────────────────────┘ |}];
  let waves = test_bits_per_cycle 12 in
  Waveform.print ~display_height:30 ~display_width:90 ~wave_width:3 ~display_rules waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────┐
    │clock             ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───│
    │                  ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   │
    │clear             ││────────┐                                                           │
    │                  ││        └───────────────────────────────────────────────────────    │
    │                  ││────────────────────────┬───────────────────────────────────────    │
    │advance_bits      ││ 0                      │12                                         │
    │                  ││────────────────────────┴───────────────────────────────────────    │
    │                  ││────────┬───────┬───────┬───────────────┬───────┬───────┬───────    │
    │bits_in           ││ 0000   │0101   │0202   │0303           │0404   │0505   │0606       │
    │                  ││────────┴───────┴───────┴───────────────┴───────┴───────┴───────    │
    │bits_in_available ││        ┌───────────────────────────────────────────────────────    │
    │                  ││────────┘                                                           │
    │                  ││────────────────┬───────────────┬───────┬───────┬───────┬───────    │
    │bits              ││ 0000           │0101           │2020   │0302   │4030   │0404       │
    │                  ││────────────────┴───────────────┴───────┴───────┴───────┴───────    │
    │bits_out_available││                ┌───────────────────────────────────────────────    │
    │                  ││────────────────┘                                                   │
    │read_bits_in      ││────────────────────────────────┐       ┌───────────────────────    │
    │                  ││                                └───────┘                           │
    │                  ││────────────────┬───────┬───────┬───────┬───────┬───────┬───────    │
    │num_bits_available││ 0              │16     │32     │36     │24     │28     │32         │
    │                  ││────────────────┴───────┴───────┴───────┴───────┴───────┴───────    │
    │                  ││────────────────┬───────┬───────┬───────────────┬───────┬───────    │
    │bits_buffer       ││ 000000000000   │010100.│020201.│030302020101   │040403.│050504.    │
    │                  ││────────────────┴───────┴───────┴───────────────┴───────┴───────    │
    │                  ││────────────────┬───────┬───────┬───────┬───────┬───────┬───────    │
    │bits_buffer_offset││ 48             │32     │16     │12     │24     │20     │16         │
    │                  ││────────────────┴───────┴───────┴───────┴───────┴───────┴───────    │
    └──────────────────┘└────────────────────────────────────────────────────────────────────┘ |}]
;;

let test_random_bits_per_cycle num_words_in =
  let sim, waves, inputs, outputs = create_sim () in
  let bits_in =
    Array.init num_words_in ~f:(fun _ -> Bits.of_int ~width:16 (Random.int (1 lsl 160)))
  in
  let total_bits_in = num_words_in * Bits.width !(inputs.bits_in) in
  let counter_in = ref 0 in
  let bits_out = ref [] in
  let bits_left = ref total_bits_in in
  let cycle () =
    let available = Bits.to_int !(outputs.bits_out_available) <> 0 in
    let data = !(outputs.bits) in
    let read_in = Bits.to_int !(outputs.read_bits_in) <> 0 in
    Cyclesim.cycle sim;
    if read_in
    then (
      (inputs.bits_in
         := try bits_in.(!counter_in + 1) with
            | _ -> Bits.zero (Bits.width !(inputs.bits_in)));
      Int.incr counter_in);
    if available
    then (
      let num_bits = Bits.to_int !(inputs.advance_bits) in
      if num_bits <> 0 then bits_out := Bits.(data.:[num_bits - 1, 0]) :: !bits_out;
      bits_left := !bits_left - num_bits);
    inputs.advance_bits := Bits.of_int ~width:5 (min !bits_left (Random.int 17))
  in
  inputs.clocking.clear := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clocking.clear := Bits.gnd;
  inputs.bits_in_available := Bits.vdd;
  inputs.bits_in := bits_in.(0);
  while !bits_left <> 0 do
    cycle ()
  done;
  (* repack the results and compare to the inputs *)
  let result =
    Bits.concat_lsb (List.rev !bits_out)
    |> Bits.split_lsb ~part_width:16 ~exact:false
    |> Array.of_list
  in
  (* Enable for debugging *)
  if false
  then
    print_s
      [%message
        (bits_in : Bits.t array) (bits_out : Bits.t list ref) (result : Bits.t array)];
  (* fail if the inputs do not the equal the results *)
  assert ([%equal: Bits.t array] bits_in result);
  waves
;;

let%expect_test "random bits per cycle" =
  let waves = test_random_bits_per_cycle 1_000 in
  Waveform.print ~display_height:30 ~display_width:90 ~wave_width:0 ~display_rules waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────┐
    │clock             ││┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐│
    │                  ││ └┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└│
    │clear             ││──┐                                                                 │
    │                  ││  └─────────────────────────────────────────────────────────────────│
    │                  ││────┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─│
    │advance_bits      ││ 0  │2│4│8│.│1│8│.│4│1│.│5│.│9│.│9│6│.│2│7│.│9│.│.│2│.│6│4│.│6│4│9│3│
    │                  ││────┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─│
    │                  ││──┬─┬─┬─┬─────┬───┬─┬─────┬───┬─┬─┬─────┬─┬───┬─┬─┬─────┬─┬───┬─────│
    │bits_in           ││ .│.│.│.│D000 │EF.│.│7E94 │BA.│.│.│B9FF │.│DE.│.│.│52F1 │.│C9.│9498 │
    │                  ││──┴─┴─┴─┴─────┴───┴─┴─────┴───┴─┴─┴─────┴─┴───┴─┴─┴─────┴─┴───┴─────│
    │bits_in_available ││  ┌─────────────────────────────────────────────────────────────────│
    │                  ││──┘                                                                 │
    │                  ││────┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─│
    │bits              ││ 00.│.│.│.│.│.│.│.│.│.│.│.│.│.│.│.│.│.│.│.│.│.│.│.│.│.│.│.│.│.│.│.│.│
    │                  ││────┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─│
    │bits_out_available││    ┌───────────────────────────────────────────────────────────────│
    │                  ││────┘                                                               │
    │read_bits_in      ││────────┐   ┌─┐ ┌───┐   ┌─┐ ┌─────┐   ┌───┐ ┌─────┐   ┌───┐ ┌─┐   ┌─│
    │                  ││        └───┘ └─┘   └───┘ └─┘     └───┘   └─┘     └───┘   └─┘ └───┘ │
    │                  ││────┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬───┬─┬─┬─┬─┬─┬─┬─┬─┬─│
    │num_bits_available││ 0  │.│.│.│.│.│.│.│.│.│.│.│.│.│.│.│.│.│.│.│.│.│32 │.│.│.│.│.│.│.│.│.│
    │                  ││────┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴───┴─┴─┴─┴─┴─┴─┴─┴─┴─│
    │                  ││────┬─┬─┬─────┬───┬─┬─────┬───┬─┬─┬─────┬─┬───┬─┬─┬─────┬─┬───┬─────│
    │bits_buffer       ││ 00.│.│.│8786.│D0.│.│3397.│7E.│.│.│E3AB.│.│9E.│.│.│9523.│.│03.│C9D1.│
    │                  ││────┴─┴─┴─────┴───┴─┴─────┴───┴─┴─┴─────┴─┴───┴─┴─┴─────┴─┴───┴─────│
    │                  ││────┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬───┬─┬─┬─┬─┬─┬─┬─┬─┬─│
    │bits_buffer_offset││ 48 │.│.│6│.│.│.│.│.│5│6│.│.│.│.│.│9│.│.│.│7│.│16 │.│.│.│.│5│.│6│.│.│
    │                  ││────┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴───┴─┴─┴─┴─┴─┴─┴─┴─┴─│
    └──────────────────┘└────────────────────────────────────────────────────────────────────┘ |}]
;;
