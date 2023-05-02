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
  module Coefs = struct
    type t = int array

    let sexp_of_t = Util.sexp_of_coef_block
  end

  module Pixels = struct
    type t = int array

    let sexp_of_t = Util.sexp_of_pixel_block
  end

  module Decoded = struct
    type t =
      { dequant : Coefs.t
      ; idct : Coefs.t
      ; recon : Pixels.t
      ; error : Pixels.t
      }
    [@@deriving sexp_of]

    let create () =
      { dequant = Array.create ~len:64 0
      ; idct = Array.create ~len:64 0
      ; recon = Array.create ~len:64 0
      ; error = Array.create ~len:64 0
      }
    ;;
  end

  type t =
    { mutable x_pos : int
    ; mutable y_pos : int
    ; input_pixels : Pixels.t
    ; fdct : Coefs.t
    ; quant : Coefs.t
    ; mutable dc_pred : int
    ; mutable rle : Rle.t list
    ; decoded : Decoded.t option
    }
  [@@deriving sexp_of]

  let create compute_reconstruction_error =
    { x_pos = 0
    ; y_pos = 0
    ; input_pixels = Array.create ~len:64 0
    ; fdct = Array.create ~len:64 0
    ; quant = Array.create ~len:64 0
    ; dc_pred = 0
    ; rle = []
    ; decoded = (if compute_reconstruction_error then Some (Decoded.create ()) else None)
    }
  ;;
end

let level_shifted_input_block plane (block : Block.t) ~x_pos ~y_pos =
  for y = 0 to 7 do
    for x = 0 to 7 do
      let k = (y * 8) + x in
      let p = Char.to_int Plane.(plane.![x + x_pos, y + y_pos]) in
      block.input_pixels.(k) <- p;
      block.fdct.(k) <- p - 128
    done
  done
;;

let fdct (block : Block.t) = Dct.Chen.forward_8x8 block.fdct

let idct (block : Block.t) =
  Option.iter block.decoded ~f:(fun block -> Dct.Chen.inverse_8x8 block.idct)
;;

let quant_and_scale fdct qnt =
  (* the forward chen transform is scaled by 4. *)
  if fdct < 0 then (fdct - (qnt * 2)) / (qnt * 4) else (fdct + (qnt * 2)) / (qnt * 4)
;;

let quant (block : Block.t) ~table =
  for i = 0 to 63 do
    block.quant.(Zigzag.forward.(i))
      <- quant_and_scale block.fdct.(i) table.(Zigzag.forward.(i))
  done
;;

let dequant (block : Block.t) ~table =
  Option.iter block.decoded ~f:(fun dec_block ->
      for i = 0 to 63 do
        let c = block.quant.(i) * table.(i) in
        dec_block.dequant.(Zigzag.inverse.(i)) <- c;
        dec_block.idct.(Zigzag.inverse.(i)) <- c
      done)
;;

let recon (block : Block.t) =
  Option.iter block.decoded ~f:(fun dec_block ->
      for i = 0 to 63 do
        dec_block.recon.(i) <- Int.max 0 (Int.min 255 (dec_block.idct.(i) + 128));
        dec_block.error.(i) <- Int.abs (dec_block.recon.(i) - block.input_pixels.(i))
      done)
;;

let rle (block : Block.t) =
  let quant = block.quant in
  let rec rle run pos =
    if pos = 63
    then (
      let value = quant.(pos) in
      [ { Rle.run; value } ])
    else (
      let value = quant.(pos) in
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
    let magnitude = magnitude ~size value in
    (* Stdio.print_s [%message (size : int) (code : Tables.dc_coef) (magnitude : int)]; *)
    Writer.put_bits writer ~stuffing:true ~value:code.bits ~bits:code.length;
    Writer.put_bits writer ~stuffing:true ~value:magnitude ~bits:size
  in
  let write_ac run value =
    let size = size value in
    let code = ac_table.(run).(size) in
    let magnitude = magnitude ~size value in
    (* Stdio.print_s
      [%message (run : int) (size : int) (code : Tables.ac_coef) (magnitude : int)]; *)
    Writer.put_bits writer ~stuffing:true ~value:code.bits ~bits:code.length;
    Writer.put_bits writer ~stuffing:true ~value:magnitude ~bits:size
  in
  let rec ac (c : Rle.t list) pos =
    match c with
    | [] -> assert (pos = 64)
    | [ { run; value = 0 } ] ->
      (* end of block *)
      write_ac 0 0;
      assert (pos + run + 1 = 64)
    | { run; value } :: tl ->
      assert (not (run = 0 && value = 0));
      let rec runs run =
        if run >= 16
        then (
          write_ac 15 0;
          runs (run - 16))
        else write_ac run value
      in
      runs run;
      ac tl (pos + run + 1)
  in
  match block.rle with
  | { run = 0; value } :: tl ->
    write_dc value;
    ac tl 1
  | _ -> failwith "no or invalid dc coef?"
;;

let encode_block plane block ~writer ~dc_table ~ac_table ~qnt_table =
  level_shifted_input_block plane block ~x_pos:block.x_pos ~y_pos:block.y_pos;
  fdct block;
  quant block ~table:qnt_table;
  rle block;
  write_bits block ~writer ~dc_table ~ac_table;
  (* for debugging *)
  dequant block ~table:qnt_table;
  idct block;
  recon block
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

module Sequenced = struct
  type t =
    { frame : Frame.t
    ; writer : Writer.t
    ; blocks : Block.t array
    ; qnt_luma : int array
    ; qnt_chroma : int array
    ; dc_luma : Tables.dc_coef array
    ; ac_luma : Tables.ac_coef array array
    ; dc_chroma : Tables.dc_coef array
    ; ac_chroma : Tables.ac_coef array array
    }

  let create_and_write_header
      ?(compute_reconstruction_error = false)
      ~frame
      ~quality
      ~writer
      ()
    =
    let width = Frame.width frame in
    let height = Frame.height frame in
    assert (width % 16 = 0);
    assert (height % 16 = 0);
    let blocks = Array.init 3 ~f:(fun _ -> Block.create compute_reconstruction_error) in
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
    { frame
    ; writer
    ; blocks
    ; qnt_luma
    ; qnt_chroma
    ; dc_luma
    ; ac_luma
    ; dc_chroma
    ; ac_chroma
    }
  ;;

  let encode_420_seq
      { frame
      ; writer
      ; blocks
      ; qnt_luma
      ; qnt_chroma
      ; dc_luma
      ; ac_luma
      ; dc_chroma
      ; ac_chroma
      }
    =
    let width = Frame.width frame in
    let height = Frame.height frame in
    Sequence.init (height / 16) ~f:(fun y_mb ->
        Sequence.init (width / 16) ~f:(fun x_mb ->
            let y =
              Sequence.init 2 ~f:(fun y_subblk ->
                  Sequence.init 2 ~f:(fun x_subblk ->
                      let x_blk = (x_mb * 2) + x_subblk in
                      let y_blk = (y_mb * 2) + y_subblk in
                      blocks.(0).x_pos <- x_blk * 8;
                      blocks.(0).y_pos <- y_blk * 8;
                      encode_block
                        (Frame.y frame)
                        blocks.(0)
                        ~writer
                        ~dc_table:dc_luma
                        ~ac_table:ac_luma
                        ~qnt_table:qnt_luma;
                      blocks.(0)))
              |> Sequence.concat
            in
            let u =
              Sequence.init 1 ~f:(fun _ ->
                  blocks.(1).x_pos <- x_mb * 8;
                  blocks.(1).y_pos <- y_mb * 8;
                  encode_block
                    (Frame.u frame)
                    blocks.(1)
                    ~writer
                    ~dc_table:dc_chroma
                    ~ac_table:ac_chroma
                    ~qnt_table:qnt_chroma;
                  blocks.(1))
            in
            let v =
              Sequence.init 1 ~f:(fun _ ->
                  blocks.(2).x_pos <- x_mb * 8;
                  blocks.(2).y_pos <- y_mb * 8;
                  encode_block
                    (Frame.v frame)
                    blocks.(2)
                    ~writer
                    ~dc_table:dc_chroma
                    ~ac_table:ac_chroma
                    ~qnt_table:qnt_chroma;
                  blocks.(2))
            in
            Sequence.Infix.(y @ u @ v))
        |> Sequence.concat)
    |> Sequence.concat
  ;;

  let complete_and_write_eoi { writer; _ } =
    Writer.flush_with_1s writer ~stuffing:true;
    write_marker_code writer Marker_code.eoi
  ;;
end

(* Basic 420 encoder.  We'll generalize over components and scans shortly. *)
let encode_420 ~frame ~quality ~writer =
  let t = Sequenced.create_and_write_header ~frame ~quality ~writer () in
  Sequence.iter (Sequenced.encode_420_seq t) ~f:(fun _ -> ());
  Sequenced.complete_and_write_eoi t
;;

module For_testing = struct
  module Sequenced = Sequenced
end
