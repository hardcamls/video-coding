open! Core
open! Hardcaml
open Hardcaml_jpeg
open Hardcaml_jpeg_model
open! Hardcaml_waveterm

module Decoder = struct
  module Decoder = Decoder.Core
  module Reader = Util.Super_simple_bitstream_reader

  module I = struct
    type 'a t =
      { clocking : 'a Clocking.t
      ; start : 'a
      ; markers : 'a Vld.Core.All_markers.t
      ; pixel_read_address : 'a [@bits 6]
      ; pixel_read_enable : 'a
      ; output_done : 'a
      }
    [@@deriving sexp_of, hardcaml ~rtlmangle:"$"]
  end

  module O = Decoder.O

  let create ~bits scope (i : _ I.t) =
    let reader = Reader.O.Of_signal.wires () in
    let decoder =
      Decoder.hierarchical
        scope
        { Decoder.I.clocking = i.clocking
        ; start = i.start
        ; markers = i.markers
        ; bits = reader.bits
        ; pixel_read_address = i.pixel_read_address
        ; pixel_read_enable = i.pixel_read_enable
        ; output_done = i.output_done
        }
    in
    Reader.O.Of_signal.assign
      reader
      (Reader.create
         ~bits
         scope
         { Reader.I.clocking = i.clocking; read_bits = decoder.read_bits });
    decoder
  ;;
end

module Sim = Cyclesim.With_interface (Decoder.I) (Decoder.O)

let ( <--. ) a b = a := Bits.of_int ~width:(Bits.width !a) b

let reconstruct block_number frame pixels =
  let pixels = Array.of_list pixels in
  let macroblock = block_number / 6 in
  let subblock = block_number % 6 in
  let width = Frame.width frame in
  let y_pos = macroblock / (width / 16) in
  let x_pos = macroblock % (width / 16) in
  print_s
    [%message
      (macroblock : int) (subblock : int) (width : int) (x_pos : int) (y_pos : int)];
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

let test ?(waves = true) ?num_blocks_to_decode jpeg =
  let headers, entropy_bits = Util.headers_and_entropy_coded_segment jpeg in
  let width = (Model.Header.frame headers |> Option.value_exn).width in
  let height = (Model.Header.frame headers |> Option.value_exn).height in
  let total_luma_blocks = width / 8 * (height / 8) in
  let total_blocks = total_luma_blocks + (total_luma_blocks / 2) in
  let num_blocks_to_decode = Option.value ~default:total_blocks num_blocks_to_decode in
  print_s [%message (width : int) (height : int) (total_blocks : int)];
  let frame = Frame.create ~chroma_subsampling:C420 ~width ~height in
  let sim =
    Sim.create
      ~config:{ Cyclesim.Config.trace_all with deduplicate_signals = false }
      (Decoder.create
         ~bits:entropy_bits
         (Scope.create ~auto_label_hierarchical_ports:true ~flatten_design:true ()))
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
  inputs.output_done := Bits.vdd;
  inputs.clocking.clear := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clocking.clear := Bits.gnd;
  (* configure core *)
  let huffman_tables = Model.Header.huffman_tables headers in
  Util.load_huffman_tables
    ~cycle:(fun () -> Cyclesim.cycle sim)
    inputs.markers.dht
    huffman_tables;
  let quant_tables = Model.Header.quant_tables headers in
  Util.load_quant_tables
    ~cycle:(fun () -> Cyclesim.cycle sim)
    inputs.markers.dqt
    quant_tables;
  (* run core *)
  inputs.start := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.start := Bits.gnd;
  let output_a_block () =
    let num_cycles = ref 0 in
    let cycle () =
      if !num_cycles > 2_000 then raise_s [%message "Took too long to decode block"];
      Cyclesim.cycle sim;
      Int.incr num_cycles
    in
    let pixels = ref [] in
    let add_pixel () = pixels := Bits.to_int !(outputs.pixel) :: !pixels in
    while not (Bits.to_bool !(outputs.start_output)) do
      cycle ()
    done;
    inputs.output_done := Bits.gnd;
    cycle ();
    inputs.pixel_read_enable := Bits.vdd;
    for i = 0 to 63 do
      inputs.pixel_read_address <--. i;
      cycle ();
      add_pixel ()
    done;
    inputs.pixel_read_enable := Bits.gnd;
    cycle ();
    inputs.output_done := Bits.vdd;
    cycle ();
    List.rev !pixels
  in
  try
    for block_number = 0 to num_blocks_to_decode - 1 do
      let pixels = output_a_block () in
      reconstruct block_number frame pixels;
      print_s [%message (block_number : int) (pixels : Int.t list)]
    done;
    for _ = 0 to 10 do
      Cyclesim.cycle sim
    done;
    frame, waves
  with
  | e ->
    print_s [%message "Failed" (e : exn)];
    frame, waves
;;

let%expect_test "test decoder" =
  let _, waves = test ~waves:true ~num_blocks_to_decode:6 "Mouse480.jpg" in
  Option.iter
    waves
    ~f:(Waveform.print ~display_height:50 ~display_width:120 ~wave_width:2 ~start_cycle:0);
  [%expect
    {|
    ((width 480) (height 320) (total_blocks 2400))
    ((macroblock 0) (subblock 0) (width 480) (x_pos 0) (y_pos 0))
    ((block_number 0)
     (pixels
      (224 222 219 214 209 205 202 200 228 226 223 219 215 211 207 206 233 232
       230 226 223 219 217 215 238 237 235 233 230 228 226 225 239 239 238 236
       235 234 233 232 237 237 237 236 236 236 236 235 233 233 234 234 235 235
       235 236 230 231 231 232 233 234 235 235)))
    ((macroblock 0) (subblock 1) (width 480) (x_pos 0) (y_pos 0))
    ((block_number 1)
     (pixels
      (191 191 191 191 191 191 191 191 207 207 207 207 207 207 207 207 227 227
       227 227 227 227 227 227 239 239 239 239 239 239 239 239 240 240 240 240
       240 240 240 240 238 238 238 238 238 238 238 238 240 240 240 240 240 240
       240 240 242 242 242 242 242 242 242 242)))
    ((macroblock 0) (subblock 2) (width 480) (x_pos 0) (y_pos 0))
    ((block_number 2)
     (pixels
      (198 203 212 221 228 232 233 234 179 183 189 198 208 218 226 230 165 165
       166 172 183 197 211 220 175 170 164 162 168 181 195 205 202 194 182 173
       171 176 185 192 222 215 204 193 186 184 185 187 222 220 215 209 203 197
       193 191 214 215 216 216 213 207 201 198)))
    ((macroblock 0) (subblock 3) (width 480) (x_pos 0) (y_pos 0))
    ((block_number 3)
     (pixels
      (215 224 236 246 249 244 236 229 215 221 229 237 241 242 240 238 210 211
       213 218 225 233 241 245 196 193 190 192 200 215 230 240 180 175 169 169
       178 194 212 224 171 167 162 161 168 182 197 207 174 171 168 168 173 181
       190 196 179 178 177 178 181 185 190 193)))
    ((macroblock 0) (subblock 4) (width 480) (x_pos 0) (y_pos 0))
    ((block_number 4)
     (pixels
      (128 128 128 128 128 128 128 128 128 128 128 128 128 128 128 128 128 128
       128 128 128 128 128 128 128 128 128 128 128 128 128 128 128 128 128 128
       128 128 128 128 128 128 128 128 128 128 128 128 128 128 128 128 128 128
       128 128 128 128 128 128 128 128 128 128)))
    ((macroblock 0) (subblock 5) (width 480) (x_pos 0) (y_pos 0))
    ((block_number 5)
     (pixels
      (123 123 123 123 123 123 123 123 123 123 123 123 123 123 123 123 123 123
       123 123 123 123 123 123 123 123 123 123 123 123 123 123 123 123 123 123
       123 123 123 123 123 123 123 123 123 123 123 123 123 123 123 123 123 123
       123 123 123 123 123 123 123 123 123 123)))
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────────────────────────┐
    │clocking$clear    ││──────┐                                                                                           │
    │                  ││      └───────────────────────────────────────────────────────────────────────────────────────────│
    │clocking$clock    ││                                                                                                  │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││────────────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─│
    │markers$dht$code  ││ 0000       │0002 │0006 │000E │001C │0038 │0070 │00E0 │01C0 │0380 │0700 │0E00 │1C00 │3800 │7000 │E│
    │                  ││────────────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─│
    │                  ││────────────┬─────┬─────┬─────────────────────────────────────────────────────────────────────────│
    │markers$dht$code_b││ 0000       │0001 │0002 │0003                                                                     │
    │                  ││────────────┴─────┴─────┴─────────────────────────────────────────────────────────────────────────│
    │                  ││────────────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─│
    │markers$dht$code_l││ 0          │1    │2    │3    │4    │5    │6    │7    │8    │9    │A    │B    │C    │D    │E    │F│
    │                  ││────────────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─│
    │markers$dht$code_w││      ┌───────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││──────┘                                                                                           │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │markers$dht$data  ││ 00                                                                                               │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │markers$dht$data_a││ 0000                                                                                             │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │markers$dht$data_w││                                                                                                  │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││──────┬───────────────────────────────────────────────────────────────────────────────────────────│
    │markers$dht$hdr$de││ 0    │1                                                                                          │
    │                  ││──────┴───────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││──────┬───────────────────────────────────────────────────────────────────────────────────────────│
    │markers$dht$hdr$ta││ 0    │1                                                                                          │
    │                  ││──────┴───────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││──────┬─────────────────┬─────────────────────────────────────────────────────────────────────────│
    │markers$dht$num_co││ 00   │01               │00                                                                       │
    │                  ││──────┴─────────────────┴─────────────────────────────────────────────────────────────────────────│
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │markers$dqt$elemen││ 0000                                                                                             │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │markers$dqt$elemen││ 00                                                                                               │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │markers$dqt$elemen││                                                                                                  │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │markers$dqt$table_││ 0                                                                                                │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │output_done       ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││                                                                                                  │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    │pixel_read_address││ 00                                                                                               │
    │                  ││──────────────────────────────────────────────────────────────────────────────────────────────────│
    └──────────────────┘└──────────────────────────────────────────────────────────────────────────────────────────────────┘ |}]
;;
