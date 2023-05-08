open! Base
open Hardcaml_video_common

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

let write_sof writer ~width ~height components =
  write_marker_code writer Marker_code.sof0;
  Markers.Sof.encode
    writer
    { Markers.Sof.length = 0
    ; sample_precision = 8
    ; width
    ; height
    ; number_of_components = Array.length components
    ; components
    }
;;

let write_sos writer scan_components =
  write_marker_code writer Marker_code.sos;
  Markers.Sos.encode
    writer
    { Markers.Sos.length = 0
    ; number_of_image_components = Array.length scan_components
    ; scan_components
    ; start_of_predictor_selection = 0
    ; end_of_predictor_selection = 63
    ; successive_approximation_bit_high = 0
    ; successive_approximation_bit_low = 0
    }
;;

module Identified = struct
  type identifier = int

  type 'a t =
    { identifier : identifier
    ; data : 'a
    }

  let find which_table id identified =
    match Array.find identified ~f:(fun { identifier; data = _ } -> id = identifier) with
    | None ->
      let ids = Array.map identified ~f:(fun { identifier; _ } -> identifier) in
      raise_s
        [%message
          "Failed to find identifier" (which_table : string) (id : int) (ids : int array)]
    | Some x -> x.data
  ;;

  let map ~f = Array.map ~f:(fun { identifier; data } -> { identifier; data = f data })
end

module Parameters = struct
  type scan_component =
    { quant_table : Identified.identifier
    ; dc_huffman_table : Identified.identifier
    ; ac_huffman_table : Identified.identifier
    ; component : Identified.identifier
    ; horizontal_sampling_factor : int
    ; vertical_sampling_factor : int
    }

  type t =
    { width : int
    ; height : int
    ; quant_tables : int array Identified.t array
    ; dc_huffman_tables : Tables.Specification.t Identified.t array
    ; ac_huffman_tables : Tables.Specification.t Identified.t array
    ; scan_components : scan_component array
    }

  let yuv ~width ~height ~quality scales =
    let qnt_luma = Quant_tables.scale Quant_tables.luma quality in
    let qnt_chroma = Quant_tables.scale Quant_tables.chroma quality in
    { width
    ; height
    ; quant_tables =
        [| { identifier = 0; data = qnt_luma }; { identifier = 1; data = qnt_chroma } |]
    ; dc_huffman_tables =
        [| { identifier = 0; data = Tables.Default.dc_luma }
         ; { identifier = 1; data = Tables.Default.dc_chroma }
        |]
    ; ac_huffman_tables =
        [| { identifier = 0; data = Tables.Default.ac_luma }
         ; { identifier = 1; data = Tables.Default.ac_chroma }
        |]
    ; scan_components =
        [| { quant_table = 0
           ; dc_huffman_table = 0
           ; ac_huffman_table = 0
           ; component = 1
           ; horizontal_sampling_factor = scales.(0)
           ; vertical_sampling_factor = scales.(1)
           }
         ; { quant_table = 1
           ; dc_huffman_table = 1
           ; ac_huffman_table = 1
           ; component = 2
           ; horizontal_sampling_factor = scales.(2)
           ; vertical_sampling_factor = scales.(3)
           }
         ; { quant_table = 1
           ; dc_huffman_table = 1
           ; ac_huffman_table = 1
           ; component = 3
           ; horizontal_sampling_factor = scales.(4)
           ; vertical_sampling_factor = scales.(5)
           }
        |]
    }
  ;;

  let c420 = yuv [| 2; 2; 1; 1; 1; 1 |]
  let c422 = yuv [| 2; 2; 1; 2; 1; 2 |]
  let c444 = yuv [| 1; 1; 1; 1; 1; 1 |]

  let monochrome ~width ~height ~quality =
    let qnt_luma = Quant_tables.scale Quant_tables.luma quality in
    { width
    ; height
    ; quant_tables = [| { identifier = 0; data = qnt_luma } |]
    ; dc_huffman_tables = [| { identifier = 0; data = Tables.Default.dc_luma } |]
    ; ac_huffman_tables = [| { identifier = 0; data = Tables.Default.ac_luma } |]
    ; scan_components =
        [| { quant_table = 0
           ; dc_huffman_table = 0
           ; ac_huffman_table = 0
           ; component = 1
           ; horizontal_sampling_factor = 1
           ; vertical_sampling_factor = 1
           }
        |]
    }
  ;;
end

let write_headers
    ~params:
      { Parameters.width
      ; height
      ; quant_tables
      ; dc_huffman_tables
      ; ac_huffman_tables
      ; scan_components
      }
    ~writer
  =
  write_marker_code writer Marker_code.soi;
  write_app0 writer "Hardcaml JPEG.";
  Array.iter quant_tables ~f:(fun { identifier; data } ->
      write_dqt writer ~table_identifier:identifier data);
  write_sof
    writer
    ~width
    ~height
    (Array.map
       scan_components
       ~f:(fun
            { quant_table
            ; component
            ; horizontal_sampling_factor
            ; vertical_sampling_factor
            ; _
            }
          ->
         { Markers.Component.identifier = component
         ; horizontal_sampling_factor
         ; vertical_sampling_factor
         ; quantization_table_identifier = quant_table
         }));
  Array.iter dc_huffman_tables ~f:(fun { identifier; data } ->
      write_dht writer ~table_class:0 ~destination_identifier:identifier data);
  Array.iter ac_huffman_tables ~f:(fun { identifier; data } ->
      write_dht writer ~table_class:1 ~destination_identifier:identifier data);
  write_sos
    writer
    (Array.map
       scan_components
       ~f:(fun { component; dc_huffman_table; ac_huffman_table; _ } ->
         { Markers.Scan_component.selector = component
         ; dc_coef_selector = dc_huffman_table
         ; ac_coef_selector = ac_huffman_table
         }))
;;

type scan =
  { hscale : int
  ; vscale : int
  ; plane : Plane.t
  ; block : Block.t
  ; quant_table : int array
  ; dc_huffman_table : Tables.dc_coef array
  ; ac_huffman_table : Tables.ac_coef array array
  }

type t =
  { params : Parameters.t
  ; scans : scan list
  ; writer : Writer.t
  }
[@@defiving fields]

let create ?(compute_reconstruction_error = false) ~(params : Parameters.t) ~writer () =
  let dc_huffman_tables =
    Identified.map params.dc_huffman_tables ~f:Tables.Encoder.dc_table
  in
  let ac_huffman_tables =
    Identified.map params.ac_huffman_tables ~f:Tables.Encoder.ac_table
  in
  let max_hscale, max_vscale =
    ( Array.fold params.scan_components ~init:0 ~f:(fun acc c ->
          max acc c.horizontal_sampling_factor)
    , Array.fold params.scan_components ~init:0 ~f:(fun acc c ->
          max acc c.vertical_sampling_factor) )
  in
  let scans =
    List.init (Array.length params.scan_components) ~f:(fun i ->
        let scan = params.scan_components.(i) in
        let width =
          Int.round_up ~to_multiple_of:(8 * scan.horizontal_sampling_factor) params.width
        in
        let height =
          Int.round_up ~to_multiple_of:(8 * scan.vertical_sampling_factor) params.height
        in
        let width = width * scan.horizontal_sampling_factor / max_hscale in
        let height = height * scan.vertical_sampling_factor / max_vscale in
        { hscale = scan.horizontal_sampling_factor
        ; vscale = scan.vertical_sampling_factor
        ; block = Block.create compute_reconstruction_error
        ; plane = Plane.create ~width ~height
        ; quant_table = Identified.find "quant" scan.quant_table params.quant_tables
        ; dc_huffman_table =
            Identified.find "dc_huffman" scan.dc_huffman_table dc_huffman_tables
        ; ac_huffman_table =
            Identified.find "ac_huffman" scan.ac_huffman_table ac_huffman_tables
        })
  in
  { params; scans; writer }
;;

let get_plane t idx = (List.nth_exn t.scans idx).plane

let encode_seq (t : t) =
  let mbs_wide, mbs_high =
    match t.scans with
    | { hscale; vscale; plane; _ } :: _ ->
      Plane.width plane / (8 * hscale), Plane.height plane / (8 * vscale)
    | [] -> raise_s [%message "Image has no scans"]
  in
  let scans = Sequence.of_list t.scans in
  let inner_blocks ~x_mb ~y_mb scan =
    Sequence.init scan.vscale ~f:(fun y_subblk ->
        Sequence.init scan.hscale ~f:(fun x_subblk ->
            let x_blk = (x_mb * scan.hscale) + x_subblk in
            let y_blk = (y_mb * scan.vscale) + y_subblk in
            scan.block.x_pos <- x_blk * 8;
            scan.block.y_pos <- y_blk * 8;
            encode_block
              scan.plane
              scan.block
              ~writer:t.writer
              ~dc_table:scan.dc_huffman_table
              ~ac_table:scan.ac_huffman_table
              ~qnt_table:scan.quant_table;
            scan.block))
    |> Sequence.concat
  in
  Sequence.init mbs_high ~f:(fun y_mb ->
      Sequence.init mbs_wide ~f:(fun x_mb ->
          Sequence.map scans ~f:(inner_blocks ~x_mb ~y_mb) |> Sequence.concat)
      |> Sequence.concat)
  |> Sequence.concat
;;

let complete_and_write_eoi { writer; _ } =
  Writer.flush_with_1s writer ~stuffing:true;
  write_marker_code writer Marker_code.eoi
;;

let encode_yuv ~frame ~writer ~params =
  let t = create ~params ~writer () in
  Plane.blit ~src:(Frame.y frame) ~dst:(List.nth_exn t.scans 0).plane;
  Plane.blit ~src:(Frame.u frame) ~dst:(List.nth_exn t.scans 1).plane;
  Plane.blit ~src:(Frame.v frame) ~dst:(List.nth_exn t.scans 2).plane;
  write_headers ~params ~writer;
  Sequence.iter (encode_seq t) ~f:(fun _ -> ());
  complete_and_write_eoi t
;;

let encode_420 ~frame ~quality ~writer =
  let params =
    Parameters.c420 ~width:(Frame.width frame) ~height:(Frame.height frame) ~quality
  in
  encode_yuv ~frame ~writer ~params
;;

let encode_422 ~frame ~quality ~writer =
  let params =
    Parameters.c422 ~width:(Frame.width frame) ~height:(Frame.height frame) ~quality
  in
  encode_yuv ~frame ~writer ~params
;;

let encode_444 ~frame ~quality ~writer =
  let params =
    Parameters.c444 ~width:(Frame.width frame) ~height:(Frame.height frame) ~quality
  in
  encode_yuv ~frame ~writer ~params
;;

let encode_monochrome ~frame ~quality ~writer =
  let params =
    Parameters.monochrome ~width:(Plane.width frame) ~height:(Plane.height frame) ~quality
  in
  let t = create ~params ~writer () in
  Plane.blit ~src:frame ~dst:(List.nth_exn t.scans 0).plane;
  write_headers ~params ~writer;
  Sequence.iter (encode_seq t) ~f:(fun _ -> ());
  complete_and_write_eoi t
;;
