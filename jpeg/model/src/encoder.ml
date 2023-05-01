open! Base

(* 
   - Level shift
   - FDCT
   - Quantize
   - DC-pred
   - Zigzag
   - Run-length encode
   - Huffman encode
   
*)

module Writer = Bitstream_writer

module Rle = struct
  type t =
    { run : int
    ; value : int
    }
  [@@deriving sexp_of]
end

module Block = struct
  type t =
    { level_shifted_pixels : int array
    ; fdct : int array
    ; quant : int array
    ; mutable dc_pred : int
    ; mutable rle : Rle.t list
    }
  [@@deriving sexp_of]

  let create () =
    { level_shifted_pixels = Array.create ~len:64 0
    ; fdct = Array.create ~len:64 0
    ; quant = Array.create ~len:64 0
    ; dc_pred = 0
    ; rle = []
    }
  ;;
end

let level_shifted_input_block plane (block : Block.t) ~x_pos ~y_pos =
  for y = 0 to 7 do
    for x = 0 to 7 do
      let k = (y * 8) + x in
      let p = Char.to_int Plane.(plane.![x + x_pos, y + y_pos]) - 128 in
      block.level_shifted_pixels.(k) <- p;
      block.fdct.(k) <- p
    done
  done
;;

let fdct (block : Block.t) = Dct.Chen.forward_8x8 block.fdct

let quant (block : Block.t) ~table =
  for i = 0 to 63 do
    block.quant.(i) <- (block.fdct.(i) + (table.(i) / 2)) / table.(i)
  done
;;

let rle (block : Block.t) =
  let quant = block.quant in
  let rec rle run pos =
    if pos = 63
    then [ { Rle.run; value = quant.(Zigzag.forward.(pos)) } ]
    else (
      let value = quant.(Zigzag.forward.(pos)) in
      if value <> 0 then { run; value } :: rle 0 (pos + 1) else rle (run + 1) (pos + 1))
  in
  let dc = quant.(0) in
  block.rle <- { run = 0; value = dc - block.dc_pred } :: rle 0 1;
  block.dc_pred <- dc
;;

let size value = if value = 0 then 0 else Int.floor_log2 (Int.abs value) + 1

let magnitude ~size value =
  if value >= 0 then value land ((1 lsl size) - 1) else (value - 1) land ((1 lsl size) - 1)
;;

let write_bits
    (block : Block.t)
    ~writer
    ~(dc_table : Tables.dc_coef array)
    ~(ac_table : Tables.ac_coef array array)
  =
  let write_dc value =
    let size = size value in
    let code = dc_table.(size) in
    Writer.put_bits writer ~stuffing:true ~value:code.bits ~bits:code.length;
    Writer.put_bits writer ~stuffing:true ~value:(magnitude ~size value) ~bits:size
  in
  let write_ac run value =
    let size = size value in
    let code = ac_table.(run).(size) in
    Writer.put_bits writer ~stuffing:true ~value:code.bits ~bits:code.length;
    Writer.put_bits writer ~stuffing:true ~value:(magnitude ~size value) ~bits:size
  in
  let rec ac (c : Rle.t list) =
    match c with
    | [] -> ()
    | [ { run = _; value = 0 } ] ->
      (* end of block *)
      write_ac 0 0
    | { run; value } :: tl ->
      let rec runs run =
        if run >= 16
        then (
          write_ac 15 0;
          runs (run - 16))
        else write_ac run value
      in
      runs run;
      ac tl
  in
  match block.rle with
  | { run = 0; value } :: tl ->
    write_dc value;
    ac tl
  | _ -> failwith "no or invalid dc coef?"
;;

let encode_block plane block ~writer ~x_pos ~y_pos ~dc_table ~ac_table ~qnt_table =
  level_shifted_input_block plane block ~x_pos ~y_pos;
  fdct block;
  quant block ~table:qnt_table;
  rle block;
  write_bits block ~writer ~dc_table ~ac_table
;;

let write_marker_code writer code =
  Writer.put_bits writer ~stuffing:false ~bits:8 ~value:0xff;
  Writer.put_bits writer ~stuffing:false ~bits:8 ~value:code
;;

let write_dht
    writer
    ~table_class
    ~destination_identifier
    ({ lengths; values } : Tables.Specification.t)
  =
  write_marker_code writer Marker_code.dht;
  Markers.Dht.encode
    writer
    { Markers.Dht.length = 0; table_class; destination_identifier; lengths; values }
;;

let write_dqt writer ~table_identifier qnt =
  write_marker_code writer Marker_code.dqt;
  Markers.Dqt.encode
    writer
    { Markers.Dqt.length = 0; element_precision = 8; table_identifier; elements = qnt }
;;

let write_app0 writer data =
  write_marker_code writer Marker_code.app0;
  Writer.put_bits writer ~stuffing:false ~bits:16 ~value:(2 + String.length data);
  for i = 0 to String.length data - 1 do
    Writer.put_bits writer ~stuffing:false ~bits:8 ~value:(Char.to_int data.[i])
  done
;;

let write_sof writer ~width ~height =
  write_marker_code writer Marker_code.sof0;
  Markers.Sof.encode
    writer
    { Markers.Sof.length = 0
    ; sample_precision = 8
    ; width
    ; height
    ; number_of_components = 3
    ; components =
        [| { identifier = 1
           ; horizontal_sampling_factor = 2
           ; vertical_sampling_factor = 2
           ; quantization_table_identifier = 0
           }
         ; { identifier = 2
           ; horizontal_sampling_factor = 1
           ; vertical_sampling_factor = 1
           ; quantization_table_identifier = 1
           }
         ; { identifier = 3
           ; horizontal_sampling_factor = 1
           ; vertical_sampling_factor = 1
           ; quantization_table_identifier = 1
           }
        |]
    }
;;

let write_sos writer =
  write_marker_code writer Marker_code.sos;
  Markers.Sos.encode
    writer
    { Markers.Sos.length = 0
    ; number_of_image_components = 3
    ; scan_components =
        [| { Markers.Scan_component.selector = 1
           ; dc_coef_selector = 0
           ; ac_coef_selector = 0
           }
         ; { Markers.Scan_component.selector = 2
           ; dc_coef_selector = 1
           ; ac_coef_selector = 1
           }
         ; { Markers.Scan_component.selector = 3
           ; dc_coef_selector = 1
           ; ac_coef_selector = 1
           }
        |]
    ; start_of_predictor_selection = 0
    ; end_of_predictor_selection = 0
    ; successive_approximation_bit_high = 0
    ; successive_approximation_bit_low = 0
    }
;;

let write_headers
    writer
    ~width
    ~height
    ~dc_luma
    ~ac_luma
    ~dc_chroma
    ~ac_chroma
    ~qnt_luma
    ~qnt_chroma
  =
  write_marker_code writer Marker_code.soi;
  write_app0 writer "Hardcaml JPEG.";
  write_dqt writer ~table_identifier:0 qnt_luma;
  write_dqt writer ~table_identifier:1 qnt_chroma;
  write_sof writer ~width ~height;
  write_dht writer ~table_class:0 ~destination_identifier:0 dc_luma;
  write_dht writer ~table_class:0 ~destination_identifier:1 dc_chroma;
  write_dht writer ~table_class:1 ~destination_identifier:0 ac_luma;
  write_dht writer ~table_class:1 ~destination_identifier:1 ac_chroma;
  write_sos writer
;;

(* Basic 420 encoder.  We'll generalize over components and scans shortly. *)
let encode_420 ~frame ~quality ~writer =
  let width = Frame.width frame in
  let height = Frame.height frame in
  assert (width % 16 = 0);
  assert (height % 16 = 0);
  let blocks = Array.init 3 ~f:(fun _ -> Block.create ()) in
  let qnt_luma = Quant_tables.scale Quant_tables.luma quality in
  let qnt_chroma = Quant_tables.scale Quant_tables.chroma quality in
  write_headers
    writer
    ~width
    ~height
    ~dc_luma:Tables.Default.dc_luma
    ~ac_luma:Tables.Default.ac_luma
    ~dc_chroma:Tables.Default.dc_chroma
    ~ac_chroma:Tables.Default.ac_chroma
    ~qnt_luma
    ~qnt_chroma;
  let dc_luma = Tables.Encoder.dc_table Tables.Default.dc_luma in
  let ac_luma = Tables.Encoder.ac_table Tables.Default.ac_luma in
  let dc_chroma = Tables.Encoder.dc_table Tables.Default.dc_chroma in
  let ac_chroma = Tables.Encoder.ac_table Tables.Default.ac_chroma in
  for y_mb = 0 to height / 16 do
    for x_mb = 0 to width / 16 do
      (* luma *)
      for y_subblk = 0 to 1 do
        for x_subblk = 0 to 1 do
          let x_blk = (x_mb * 2) + x_subblk in
          let y_blk = (y_mb * 2) + y_subblk in
          encode_block
            (Frame.y frame)
            blocks.(0)
            ~writer
            ~x_pos:(x_blk * 8)
            ~y_pos:(y_blk * 8)
            ~dc_table:dc_luma
            ~ac_table:ac_luma
            ~qnt_table:qnt_luma
        done
      done;
      (* chroma *)
      encode_block
        (Frame.u frame)
        blocks.(1)
        ~writer
        ~x_pos:(x_mb * 8)
        ~y_pos:(y_mb * 8)
        ~dc_table:dc_chroma
        ~ac_table:ac_chroma
        ~qnt_table:qnt_chroma;
      encode_block
        (Frame.v frame)
        blocks.(2)
        ~writer
        ~x_pos:(x_mb * 8)
        ~y_pos:(y_mb * 8)
        ~dc_table:dc_chroma
        ~ac_table:ac_chroma
        ~qnt_table:qnt_chroma
    done
  done;
  Writer.flush_with_1s writer ~stuffing:true;
  write_marker_code writer Marker_code.eoi
;;
