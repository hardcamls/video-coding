open! Core
open Hardcaml
open Hardcaml_waveterm
open Hardcaml_jpeg
module Decoder = Bytestream_decoder
module Sim = Cyclesim.With_interface (Decoder.I) (Decoder.O)

let display_rules =
  let module I = Display_rules.With_interface (Decoder.I) in
  let module O = Display_rules.With_interface (Decoder.O) in
  List.concat
    Display_rule.
      [ I.default ()
      ; [ port_name_is "STATE" ~wave_format:(Index Decoder.State.names) ]
      ; O.default ()
      ; [ default ]
      ]
;;

let read_header
    (inputs : _ Decoder.I.t)
    (outputs : _ Decoder.O.t)
    (outputs_before : _ Decoder.O.t)
  =
  let unref x = Bits.to_int !x in
  let components = ref [] in
  let scan_selector = ref [] in
  let quant_tables = ref [] in
  let code_words = ref [] in
  let code_data = ref [] in
  let entropy_coded_segment = ref [] in
  let cycle () =
    let sof = outputs.markers.sof in
    if Bits.to_bool !(sof.component_write)
    then components := Markers.Component.Fields.map sof.component ~f:unref :: !components;
    let sos = outputs.markers.sos in
    if Bits.to_bool !(sos.write_scan_selector)
    then
      scan_selector
        := Markers.Scan_selector.Fields.map sos.scan_selector ~f:unref :: !scan_selector;
    let dqt = outputs.markers.dqt in
    if Bits.to_bool !(dqt.element_write)
    then
      quant_tables
        := ( unref dqt.fields.table_identifier
           , unref dqt.element_address
           , unref dqt.element )
           :: !quant_tables;
    let dht = outputs.markers.dht in
    if Bits.to_bool !(dht.code.code_write)
    then
      code_words
        := ( Markers.Dht.Header.Fields.map dht.header ~f:unref
           , Markers.Dht.Code.map dht.code ~f:unref )
           :: !code_words;
    if Bits.to_bool !(dht.code_data.data_write)
    then
      code_data
        := ( Markers.Dht.Header.Fields.map dht.header ~f:unref
           , Markers.Dht.Code_data.map dht.code_data ~f:unref )
           :: !code_data;
    if Bits.to_bool !(outputs_before.bits_valid) && Bits.to_bool !(inputs.bits_ready)
    then entropy_coded_segment := Bits.to_char !(outputs.bits) :: !entropy_coded_segment
  in
  let print () =
    let sof =
      outputs.markers.sof.frame_header |> Markers.Sof.Header.Fields.map ~f:unref
    in
    let sof_components = List.rev !components in
    let sos_header =
      outputs.markers.sos.header |> Markers.Sos.Header.Fields.map ~f:unref
    in
    let sos_footer =
      outputs.markers.sos.footer |> Markers.Sos.Footer.Fields.map ~f:unref
    in
    let scan_selector = List.rev !scan_selector in
    let quant_tables =
      let q =
        List.group !quant_tables ~break:(fun (a, _, _) (b, _, _) -> a <> b)
        |> List.map ~f:(List.sort ~compare:[%compare: int * int * int])
      in
      List.map q ~f:(fun l ->
          match l with
          | [] -> failwith ""
          | (idx, _, _) :: _ ->
            let a = Array.create ~len:64 0 in
            List.iter l ~f:(fun (_, i, q) -> a.(i) <- q);
            idx, a)
    in
    let code_words =
      let c =
        List.group !code_words ~break:(fun (hdr0, _) (hdr1, _) ->
            not
              ([%compare.equal: int * int]
                 (hdr0.table_class, hdr0.destination_identifier)
                 (hdr1.table_class, hdr1.destination_identifier)))
        |> List.map
             ~f:
               (List.sort ~compare:(fun (_, (a : _ Markers.Dht.Code.t)) (_, b) ->
                    Int.compare a.code_length_minus1 b.code_length_minus1))
      in
      List.map c ~f:(fun l ->
          match l with
          | [] -> failwith ""
          | (hdr, _) :: _ -> hdr, List.map l ~f:(fun (_, data) -> data))
    in
    let code_data =
      let c =
        List.group !code_data ~break:(fun (hdr0, _) (hdr1, _) ->
            not
              ([%compare.equal: int * int]
                 (hdr0.table_class, hdr0.destination_identifier)
                 (hdr1.table_class, hdr1.destination_identifier)))
        |> List.map
             ~f:
               (List.sort ~compare:(fun (_, (a : _ Markers.Dht.Code_data.t)) (_, b) ->
                    Int.compare a.data_address b.data_address))
      in
      List.map c ~f:(fun l ->
          match l with
          | [] -> failwith ""
          | (hdr, _) :: _ -> hdr, List.map l ~f:(fun (_, data) -> data))
    in
    let sexp_of_dht_header (t : _ Markers.Dht.Header.Fields.t) =
      let class_ = t.table_class in
      let destid = t.destination_identifier in
      [%message (class_ : int) (destid : int)]
    in
    let sexp_of_code_data (t : _ Markers.Dht.Code_data.t) =
      let address = t.data_address in
      let data = t.data in
      [%message (address : int) (data : int)]
    in
    let sexp_of_code (t : _ Markers.Dht.Code.t) =
      let length = t.code_length_minus1 + 1 in
      let num_codes = t.num_codes_at_length in
      let base_address = t.code_base_address in
      let code = t.code in
      [%message (length : int) (num_codes : int) (base_address : int) (code : int)]
    in
    print_s
      [%message
        "header"
          (sof : int Markers.Sof.Header.Fields.t)
          (sof_components : int Markers.Component.Fields.t list)
          (sos_header : int Markers.Sos.Header.Fields.t)
          (scan_selector : int Markers.Scan_selector.Fields.t list)
          (sos_footer : int Markers.Sos.Footer.Fields.t)
          (quant_tables : (int * int array) list)
          (code_words : (dht_header * code list) list)
          (code_data : (dht_header * code_data list) list)]
  in
  cycle, print, fun () -> String.of_char_list (List.rev !entropy_coded_segment)
;;

let test ?(waves = false) ?(random_ready = false) jpeg =
  let jpeg = Util.load_jpeg_file jpeg in
  let bits = Hardcaml_jpeg_model.Bitstream_reader.From_string.get_buffer jpeg in
  let expected_entropy_coded_segment =
    Test_decoder_accelerator.get_entropy_coded_segment bits |> Util.remove_stuffing_bytes
  in
  let sim =
    Sim.create
      ~config:Cyclesim.Config.trace_all
      (Decoder.create
         (Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true ()))
  in
  let waves, sim =
    if waves
    then (
      let waves, sim = Waveform.create sim in
      Some waves, sim)
    else None, sim
  in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let outputs_before = Cyclesim.outputs ~clock_edge:Before sim in
  inputs.clocking.clear := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clocking.clear := Bits.gnd;
  inputs.start := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.start := Bits.gnd;
  inputs.bits_ready := Bits.vdd;
  let cycle_headers, print_headers, entropy_coded_segment =
    read_header inputs outputs outputs_before
  in
  let num_cycles = ref 0 in
  let pos = ref 0 in
  let cycle () =
    inputs.bits_ready := if random_ready then Bits.random ~width:1 else Bits.vdd;
    Cyclesim.cycle sim;
    cycle_headers ();
    if Bits.to_bool !(outputs_before.jpeg_ready)
    then (
      (inputs.jpeg_valid := Bits.vdd;
       inputs.jpeg
         := try Bits.of_char bits.[!pos] with
            | _ -> Bits.zero 8);
      Int.incr pos);
    Int.incr num_cycles
  in
  while (not (Bits.to_bool !(outputs.done_))) && !num_cycles < 20_000 do
    cycle ()
  done;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  let entropy_coded_segment = entropy_coded_segment () in
  if String.length expected_entropy_coded_segment + 2
     <> String.length entropy_coded_segment
  then
    print_s
      [%message
        "ECS length mismatch"
          (String.length expected_entropy_coded_segment : int)
          (String.length entropy_coded_segment : int)];
  if not
       (String.equal
          expected_entropy_coded_segment
          (String.subo
             ~len:(String.length entropy_coded_segment - 2)
             entropy_coded_segment))
  then (
    for i = 0 to String.length expected_entropy_coded_segment - 1 do
      if not (Char.equal expected_entropy_coded_segment.[i] entropy_coded_segment.[i])
      then
        print_s
          [%message
            "mismatch"
              (i : int)
              (entropy_coded_segment.[i] : char)
              (expected_entropy_coded_segment.[i] : char)]
    done;
    print_s
      [%message
        "ECS mismatch"
          (String.subo expected_entropy_coded_segment ~pos:6250 : String.Hexdump.t)
          (String.subo entropy_coded_segment ~pos:6250 : String.Hexdump.t)]);
  print_headers ();
  waves
;;

let%expect_test "test reader" =
  let waves = test ~waves:true ~random_ready:false "Mouse480.jpg" in
  Option.iter
    waves
    ~f:
      (Waveform.print
         ~display_width:100
         ~display_height:150
         ~wave_width:(-100)
         ~start_cycle:0
         ~display_rules);
  [%expect
    {|
    (header
     (sof
      ((length 17) (sample_precision 8) (height 320) (width 480)
       (number_of_components 3)))
     (sof_components
      (((identifier 1) (vertical_sampling_factor 2)
        (horizontal_sampling_factor 2) (quantization_table_identifier 0))
       ((identifier 2) (vertical_sampling_factor 1)
        (horizontal_sampling_factor 1) (quantization_table_identifier 1))
       ((identifier 3) (vertical_sampling_factor 1)
        (horizontal_sampling_factor 1) (quantization_table_identifier 1))))
     (sos_header ((length 12) (number_of_image_components 3)))
     (scan_selector
      (((selector 1) (dc_coef_selector 0) (ac_coef_selector 0))
       ((selector 2) (dc_coef_selector 1) (ac_coef_selector 1))
       ((selector 3) (dc_coef_selector 1) (ac_coef_selector 1))))
     (sos_footer
      ((start_of_predictor_selection 0) (end_of_predictor_selection 63)
       (successive_approximation_bit_high 0)
       (successive_approximation_bit_low 0)))
     (quant_tables
      ((1
        (43 43 45 45 60 53 60 118 65 65 118 248 165 140 165 248 248 248 248 248
         248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248
         248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248 248
         248 248 248 248 248 248 248 248))
       (0
        (40 40 28 30 35 30 25 40 35 33 35 45 43 40 48 60 100 65 60 55 55 60 123
         88 93 73 100 145 128 153 150 143 128 140 138 160 180 230 195 160 170 218
         173 138 140 200 255 203 218 238 245 255 255 255 155 193 255 255 255 250
         255 230 253 255))))
     (code_words
      ((((class_ 1) (destid 1))
        (((length 1) (num_codes 1) (base_address 0) (code 0))
         ((length 2) (num_codes 1) (base_address 1) (code 2))
         ((length 3) (num_codes 1) (base_address 2) (code 6))
         ((length 4) (num_codes 1) (base_address 3) (code 14))
         ((length 5) (num_codes 0) (base_address 3) (code 28))
         ((length 6) (num_codes 0) (base_address 3) (code 56))
         ((length 7) (num_codes 0) (base_address 3) (code 112))
         ((length 8) (num_codes 0) (base_address 3) (code 224))
         ((length 9) (num_codes 0) (base_address 3) (code 448))
         ((length 10) (num_codes 0) (base_address 3) (code 896))
         ((length 11) (num_codes 0) (base_address 3) (code 1792))
         ((length 12) (num_codes 0) (base_address 3) (code 3584))
         ((length 13) (num_codes 0) (base_address 3) (code 7168))
         ((length 14) (num_codes 0) (base_address 3) (code 14336))
         ((length 15) (num_codes 0) (base_address 3) (code 28672))
         ((length 16) (num_codes 0) (base_address 3) (code 57344))))
       (((class_ 0) (destid 1))
        (((length 1) (num_codes 1) (base_address 0) (code 0))
         ((length 2) (num_codes 1) (base_address 1) (code 2))
         ((length 3) (num_codes 1) (base_address 2) (code 6))
         ((length 4) (num_codes 1) (base_address 3) (code 14))
         ((length 5) (num_codes 0) (base_address 3) (code 28))
         ((length 6) (num_codes 0) (base_address 3) (code 56))
         ((length 7) (num_codes 0) (base_address 3) (code 112))
         ((length 8) (num_codes 0) (base_address 3) (code 224))
         ((length 9) (num_codes 0) (base_address 3) (code 448))
         ((length 10) (num_codes 0) (base_address 3) (code 896))
         ((length 11) (num_codes 0) (base_address 3) (code 1792))
         ((length 12) (num_codes 0) (base_address 3) (code 3584))
         ((length 13) (num_codes 0) (base_address 3) (code 7168))
         ((length 14) (num_codes 0) (base_address 3) (code 14336))
         ((length 15) (num_codes 0) (base_address 3) (code 28672))
         ((length 16) (num_codes 0) (base_address 3) (code 57344))))
       (((class_ 1) (destid 0))
        (((length 1) (num_codes 0) (base_address 0) (code 0))
         ((length 2) (num_codes 0) (base_address 0) (code 0))
         ((length 3) (num_codes 2) (base_address 2) (code 4))
         ((length 4) (num_codes 2) (base_address 4) (code 12))
         ((length 5) (num_codes 1) (base_address 5) (code 26))
         ((length 6) (num_codes 4) (base_address 9) (code 60))
         ((length 7) (num_codes 1) (base_address 10) (code 122))
         ((length 8) (num_codes 4) (base_address 14) (code 252))
         ((length 9) (num_codes 2) (base_address 16) (code 508))
         ((length 10) (num_codes 1) (base_address 17) (code 1018))
         ((length 11) (num_codes 4) (base_address 21) (code 2044))
         ((length 12) (num_codes 3) (base_address 24) (code 4094))
         ((length 13) (num_codes 1) (base_address 25) (code 8190))
         ((length 14) (num_codes 1) (base_address 26) (code 16382))
         ((length 15) (num_codes 0) (base_address 26) (code 32764))
         ((length 16) (num_codes 0) (base_address 26) (code 65528))))
       (((class_ 0) (destid 0))
        (((length 1) (num_codes 0) (base_address 0) (code 0))
         ((length 2) (num_codes 0) (base_address 0) (code 0))
         ((length 3) (num_codes 3) (base_address 3) (code 6))
         ((length 4) (num_codes 1) (base_address 4) (code 14))
         ((length 5) (num_codes 1) (base_address 5) (code 30))
         ((length 6) (num_codes 1) (base_address 6) (code 62))
         ((length 7) (num_codes 1) (base_address 7) (code 126))
         ((length 8) (num_codes 0) (base_address 7) (code 252))
         ((length 9) (num_codes 0) (base_address 7) (code 504))
         ((length 10) (num_codes 0) (base_address 7) (code 1008))
         ((length 11) (num_codes 0) (base_address 7) (code 2016))
         ((length 12) (num_codes 0) (base_address 7) (code 4032))
         ((length 13) (num_codes 0) (base_address 7) (code 8064))
         ((length 14) (num_codes 0) (base_address 7) (code 16128))
         ((length 15) (num_codes 0) (base_address 7) (code 32256))
         ((length 16) (num_codes 0) (base_address 7) (code 64512))))))
     (code_data
      ((((class_ 1) (destid 1))
        (((address 0) (data 0)) ((address 1) (data 0)) ((address 2) (data 17))))
       (((class_ 0) (destid 1))
        (((address 0) (data 0)) ((address 1) (data 0)) ((address 2) (data 1))))
       (((class_ 1) (destid 0))
        (((address 0) (data 0)) ((address 1) (data 0)) ((address 2) (data 1))
         ((address 3) (data 2)) ((address 4) (data 17)) ((address 5) (data 33))
         ((address 6) (data 3)) ((address 7) (data 18)) ((address 8) (data 49))
         ((address 9) (data 65)) ((address 10) (data 81)) ((address 11) (data 4))
         ((address 12) (data 19)) ((address 13) (data 34))
         ((address 14) (data 97)) ((address 15) (data 50))
         ((address 16) (data 113)) ((address 17) (data 129))
         ((address 18) (data 5)) ((address 19) (data 35))
         ((address 20) (data 66)) ((address 21) (data 145))
         ((address 22) (data 20)) ((address 23) (data 51))
         ((address 24) (data 82)) ((address 25) (data 161))))
       (((class_ 0) (destid 0))
        (((address 0) (data 0)) ((address 1) (data 0)) ((address 2) (data 1))
         ((address 3) (data 2)) ((address 4) (data 3)) ((address 5) (data 4))
         ((address 6) (data 5)))))))
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────┐
    │clock             ││╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥│
    │                  ││╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨│
    │clear             ││╥                                                                             │
    │                  ││╨──────────────────────────────────────────────────────────────────           │
    │start             ││╥                                                                             │
    │                  ││╨──────────────────────────────────────────────────────────────────           │
    │                  ││╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥           │
    │jpeg              ││║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║           │
    │                  ││╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨           │
    │jpeg_valid        ││╥──────────────────────────────────────────────────────────────────           │
    │                  ││╨                                                                             │
    │bits_ready        ││╥──────────────────────────────────────────────────────────────────           │
    │                  ││╨                                                                             │
    │                  ││╥╥╥╥────────────────╥─────╥────────╥───╥─╥────╥╥─╥──╥────────╥╥──╥╥           │
    │STATE             ││║║║║ Entropy_coded_.║ Ent.║ Entrop.║ E.║ ║ En.║║ ║ .║ Entrop.║║ .║║           │
    │                  ││╨╨╨╨────────────────╨─────╨────────╨───╨─╨────╨╨─╨──╨────────╨╨──╨╨           │
    │jpeg_ready        ││╥╥╥╥──────────────────────────────────────────────────────────────╥           │
    │                  ││╨╨╨╨                                                              ╨           │
    │done_             ││╥                                                                 ╥           │
    │                  ││╨─────────────────────────────────────────────────────────────────╨           │
    │                  ││╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥           │
    │bits              ││║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║           │
    │                  ││╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨           │
    │bits_valid        ││   ╥────────────────╥─────╥────────╥───╥─╥────╥╥─╥──╥────────╥╥──╥╥           │
    │                  ││───╨                ╨     ╨        ╨   ╨ ╨    ╨╨ ╨  ╨        ╨╨  ╨╨           │
    │                  ││─╥─────────────────────────────────────────────────────────────────           │
    │sof$length        ││ ║ 0011                                                                       │
    │                  ││─╨─────────────────────────────────────────────────────────────────           │
    │                  ││─╥─────────────────────────────────────────────────────────────────           │
    │sof$sample_precisi││ ║ 08                                                                         │
    │                  ││─╨─────────────────────────────────────────────────────────────────           │
    │                  ││─╥─────────────────────────────────────────────────────────────────           │
    │sof$height        ││ ║ 0140                                                                       │
    │                  ││─╨─────────────────────────────────────────────────────────────────           │
    │                  ││─╥─────────────────────────────────────────────────────────────────           │
    │sof$width         ││ ║ 01E0                                                                       │
    │                  ││─╨─────────────────────────────────────────────────────────────────           │
    │                  ││─╥─────────────────────────────────────────────────────────────────           │
    │sof$number_of_comp││ ║ 03                                                                         │
    │                  ││─╨─────────────────────────────────────────────────────────────────           │
    │                  ││─╥─────────────────────────────────────────────────────────────────           │
    │sof$identifier    ││ ║ 03                                                                         │
    │                  ││─╨─────────────────────────────────────────────────────────────────           │
    │                  ││─╥─────────────────────────────────────────────────────────────────           │
    │sof$vertical_sampl││ ║ 1                                                                          │
    │                  ││─╨─────────────────────────────────────────────────────────────────           │
    │                  ││─╥─────────────────────────────────────────────────────────────────           │
    │sof$horizontal_sam││ ║ 1                                                                          │
    │                  ││─╨─────────────────────────────────────────────────────────────────           │
    │                  ││─╥─────────────────────────────────────────────────────────────────           │
    │sof$quantization_t││ ║ 01                                                                         │
    │                  ││─╨─────────────────────────────────────────────────────────────────           │
    │                  ││─╥─────────────────────────────────────────────────────────────────           │
    │sof$component_addr││ ║ 0                                                                          │
    │                  ││─╨─────────────────────────────────────────────────────────────────           │
    │sof$component_writ││ ╥                                                                            │
    │                  ││─╨─────────────────────────────────────────────────────────────────           │
    │                  ││───╥───────────────────────────────────────────────────────────────           │
    │sos$length        ││ 0.║ 000C                                                                     │
    │                  ││───╨───────────────────────────────────────────────────────────────           │
    │                  ││───╥───────────────────────────────────────────────────────────────           │
    │sos$number_of_imag││ 00║ 03                                                                       │
    │                  ││───╨───────────────────────────────────────────────────────────────           │
    │                  ││───╥───────────────────────────────────────────────────────────────           │
    │sos$selector      ││ 00║ 03                                                                       │
    │                  ││───╨───────────────────────────────────────────────────────────────           │
    │                  ││───╥───────────────────────────────────────────────────────────────           │
    │sos$dc_coef_select││ 0 ║ 1                                                                        │
    │                  ││───╨───────────────────────────────────────────────────────────────           │
    │                  ││───╥───────────────────────────────────────────────────────────────           │
    │sos$ac_coef_select││ 0 ║ 1                                                                        │
    │                  ││───╨───────────────────────────────────────────────────────────────           │
    │sos$write_scan_sel││   ╥                                                                          │
    │                  ││───╨───────────────────────────────────────────────────────────────           │
    │                  ││───────────────────────────────────────────────────────────────────           │
    │sos$start_of_predi││ 00                                                                           │
    │                  ││───────────────────────────────────────────────────────────────────           │
    │                  ││───╥───────────────────────────────────────────────────────────────           │
    │sos$end_of_predict││ 00║ 3F                                                                       │
    │                  ││───╨───────────────────────────────────────────────────────────────           │
    │                  ││───╥───────────────────────────────────────────────────────────────           │
    │sos$successive_app││ 0 ║ 0                                                                        │
    │                  ││───╨───────────────────────────────────────────────────────────────           │
    │                  ││───╥───────────────────────────────────────────────────────────────           │
    │sos$successive_app││ 0 ║ 0                                                                        │
    │                  ││───╨───────────────────────────────────────────────────────────────           │
    │                  ││╥──────────────────────────────────────────────────────────────────           │
    │dqt$length        ││║ 0043                                                                        │
    │                  ││╨──────────────────────────────────────────────────────────────────           │
    │                  ││╥──────────────────────────────────────────────────────────────────           │
    │dqt$element_precis││║ 0                                                                           │
    │                  ││╨──────────────────────────────────────────────────────────────────           │
    │                  ││╥──────────────────────────────────────────────────────────────────           │
    │dqt$table_identifi││║ 1                                                                           │
    │                  ││╨──────────────────────────────────────────────────────────────────           │
    │                  ││╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥           │
    │dqt$element       ││║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║           │
    │                  ││╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨           │
    │                  ││╥╥─────────────────────────────────────────────────────────────────           │
    │dqt$element_addres││║║ 00                                                                         │
    │                  ││╨╨─────────────────────────────────────────────────────────────────           │
    │dqt$element_write ││╥╥                                                                            │
    │                  ││╨╨─────────────────────────────────────────────────────────────────           │
    │                  ││─╥╥╥───────────────────────────────────────────────────────────────           │
    │dht$hdr$length    ││ ║║║ 0016                                                                     │
    │                  ││─╨╨╨───────────────────────────────────────────────────────────────           │
    │                  ││─╥╥╥───────────────────────────────────────────────────────────────           │
    │dht$hdr$table_clas││ ║║║ 1                                                                        │
    │                  ││─╨╨╨───────────────────────────────────────────────────────────────           │
    │                  ││─╥╥╥───────────────────────────────────────────────────────────────           │
    │dht$hdr$destinatio││ ║║║ 1                                                                        │
    │                  ││─╨╨╨───────────────────────────────────────────────────────────────           │
    │                  ││─╥╥╥───────────────────────────────────────────────────────────────           │
    │dht$code_length_mi││ ║║║ 0                                                                        │
    │                  ││─╨╨╨───────────────────────────────────────────────────────────────           │
    │                  ││╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥           │
    │dht$num_codes_at_l││║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║           │
    │                  ││╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨           │
    │                  ││──╥╥───────────────────────────────────────────────────────────────           │
    │dht$code          ││ .║║ C000                                                                     │
    │                  ││──╨╨───────────────────────────────────────────────────────────────           │
    │                  ││──╥╥───────────────────────────────────────────────────────────────           │
    │dht$code_base_addr││ .║║ 0003                                                                     │
    │                  ││──╨╨───────────────────────────────────────────────────────────────           │
    │dht$code_write    ││ ╥╥╥                                                                          │
    │                  ││─╨╨╨───────────────────────────────────────────────────────────────           │
    │                  ││╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥           │
    │dht$data          ││║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║           │
    │                  ││╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨           │
    │                  ││──╥╥───────────────────────────────────────────────────────────────           │
    │dht$data_address  ││ .║║ 0003                                                                     │
    │                  ││──╨╨───────────────────────────────────────────────────────────────           │
    │dht$data_write    ││  ╥╥                                                                          │
    │                  ││──╨╨───────────────────────────────────────────────────────────────           │
    │decoder_start     ││   ╥                                                                          │
    │                  ││───╨───────────────────────────────────────────────────────────────           │
    │                  ││╥──────────────────────────────────────────────────────────────────           │
    │SKIP_COUNT        ││║ 0000                                                                        │
    │                  ││╨──────────────────────────────────────────────────────────────────           │
    │                  ││╥──────────────────────────────────────────────────────────────────           │
    │SKIP_LENGTH       ││║ 0010                                                                        │
    │                  ││╨──────────────────────────────────────────────────────────────────           │
    │                  ││╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥           │
    │dht$dhthdr$i$bits ││║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║║           │
    │                  ││╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨           │
    │dht$dhthdr$i$bits_││╥──────────────────────────────────────────────────────────────────           │
    │                  ││╨                                                                             │
    │dht$dhthdr$i$clear││╥                                                                             │
    └──────────────────┘└──────────────────────────────────────────────────────────────────────────────┘ |}]
;;
