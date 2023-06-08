open! Core
open Hardcaml
open Hardcaml_waveterm
module Encoder = Hardcaml_jpeg.Encoder_datapath
module Sim = Cyclesim.With_interface (Encoder.I) (Encoder.O)

let ( <--. ) a b = a := Bits.of_int ~width:(Bits.width !a) b
let qtable = Hardcaml_jpeg_model.Quant_tables.(scale luma 95)

let%expect_test "quant values" =
  print_s [%message (qtable : int array)];
  [%expect
    {|
    (qtable
     (2 1 1 2 2 4 5 6 1 1 1 2 3 6 6 6 1 1 2 2 4 6 7 6 1 2 2 3 5 9 8 6 2 2 4 6 7
      11 10 8 2 4 6 6 8 10 11 9 5 6 8 9 10 12 12 10 7 9 10 10 11 10 10 10)) |}]
;;

let test ?(waves = false) () =
  let sim =
    Sim.create
      ~config:Cyclesim.Config.trace_all
      (Encoder.create
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
  inputs.clocking.clear := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clocking.clear := Bits.gnd;
  (* load quantisation table *)
  inputs.quant_write.write <--. 1;
  Array.iteri qtable ~f:(fun addr q ->
      inputs.quant_write.quant <--. Hardcaml_jpeg.Quant.one_over_quant_coef q;
      inputs.quant_write.address <--. addr;
      Cyclesim.cycle sim);
  inputs.quant_write.write <--. 0;
  let start block dct vlc =
    inputs.starts.dct := Bits.of_bool dct;
    inputs.starts.vlc := Bits.of_bool vlc;
    Cyclesim.cycle sim;
    inputs.starts.dct := Bits.gnd;
    inputs.starts.vlc := Bits.gnd;
    inputs.pixels.write_enable := Bits.vdd;
    Array.iteri block ~f:(fun address data ->
        inputs.pixels.write_address <--. address;
        inputs.pixels.data <--. data;
        Cyclesim.cycle sim);
    while not (Bits.to_bool !(outputs.done_)) do
      Cyclesim.cycle sim
    done;
    inputs.pixels.write_enable := Bits.vdd
  in
  let zblk = Array.init 64 ~f:(Fn.const 0) in
  start zblk false false;
  start zblk true false;
  start zblk true true;
  for _ = 0 to 10 do
    Cyclesim.cycle sim
  done;
  waves
;;

let%expect_test "instantiate" = ignore (test ~waves:false () : Waveform.t option)
