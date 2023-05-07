open! Base
open Hardcaml_video_common
module Bits = Bitstream_reader.From_string

module Header = struct
  type t =
    { frame : Markers.Sof.t option
    ; quant_tables : Markers.Dqt.t list
    ; huffman_tables : Markers.Dht.t list
    ; restart_interval : Markers.Dri.t option
    ; scan : Markers.Sos.t option
    }
  [@@deriving sexp_of, fields]

  let empty =
    { frame = None
    ; scan = None
    ; quant_tables = []
    ; restart_interval = None
    ; huffman_tables = []
    }
  ;;

  let find_marker bits =
    Bits.align_to_byte bits;
    while Bits.get bits 8 <> 0xff do
      ()
    done
  ;;

  let skip bits =
    let len = Bits.show bits 16 in
    Bits.advance bits (len * 8)
  ;;

  (* Decode up to scan header*)
  let rec decode bits t =
    find_marker bits;
    let marker_code = Bits.get bits 8 in
    if marker_code = Marker_code.sof0
    then (
      let sof = Markers.Sof.decode bits in
      decode bits { t with frame = Some sof })
    else if marker_code = Marker_code.sos
    then (
      let sos = Markers.Sos.decode bits in
      { t with scan = Some sos })
    else if marker_code = Marker_code.dqt
    then (
      let dqt = Markers.Dqt.decode bits in
      decode bits { t with quant_tables = dqt :: t.quant_tables })
    else if marker_code = Marker_code.dht
    then (
      let dht = Markers.Dht.decode bits in
      decode bits { t with huffman_tables = dht :: t.huffman_tables })
    else if marker_code = Marker_code.dri
    then (
      let dri = Markers.Dri.decode bits in
      decode bits { t with restart_interval = Some dri })
    else if marker_code = Marker_code.soi
    then decode bits t
    else if (marker_code >= Marker_code.app0 && marker_code <= Marker_code.app15)
            || marker_code = Marker_code.com
    then (
      skip bits;
      decode bits t)
    else raise_s [%message "unsupported marker code" (marker_code : int)]
  ;;

  let decode bits = decode bits empty
end

let mag' cat code =
  if code land (1 lsl (cat - 1)) <> 0
  then (* +ve coeff *)
    code
  else (* -ve coeff *)
    (code lor (-1 lsl cat)) + 1
;;

let mag bits cat =
  if cat = 0
  then 0
  else (
    let v = Bits.get bits cat in
    mag' cat v)
;;

let dc_code bits table =
  let code = Bits.show bits (Tables.Lut.max_bits table) in
  match (Tables.Lut.lut table).(code) with
  | None -> raise_s [%message "Can't find dc code"]
  | Some { length; data } ->
    Bits.advance bits length;
    mag bits data
;;

let ac_code bits (table : Tables.ac Tables.Lut.t) =
  let code = Bits.show bits (Tables.Lut.max_bits table) in
  match (Tables.Lut.lut table).(code) with
  | None -> raise_s [%message "Can't find ac code"]
  | Some { length; data = { run; size } } ->
    Bits.advance bits length;
    mag bits size, run
;;

let _sexp_of_block blk =
  [%sexp_of: int array array]
  @@ Array.init 8 ~f:(fun i -> Array.init 8 ~f:(fun j -> blk.((i * 8) + j)))
;;

let clear_block block =
  for i = 0 to 63 do
    block.(i) <- 0
  done
;;

let huffman_decode
    ~bits
    ~coefs
    ~(dc_tab : Tables.dc Tables.Lut.t)
    ~(ac_tab : Tables.ac Tables.Lut.t)
  =
  (* dc coefficient *)
  let dc = dc_code bits dc_tab in
  coefs.(0) <- dc;
  (* ac coefficients *)
  let cof_cnt = ref 1 in
  while !cof_cnt < 64 do
    let mag, run = ac_code bits ac_tab in
    if mag = 0 && run = 0
    then cof_cnt := 64
    else (
      cof_cnt := !cof_cnt + run;
      if !cof_cnt >= 64
      then raise_s [%message "coefficient index out of range:" (!cof_cnt : int)];
      coefs.(!cof_cnt) <- mag;
      cof_cnt := !cof_cnt + 1)
  done
;;

let dequantize_dc_pred_and_inverse_zigzag ~qnt_tab ~dc_pred ~coefs ~dequant =
  let dc = coefs.(0) + dc_pred in
  dequant.(0) <- dc * qnt_tab.(0);
  for i = 1 to 63 do
    dequant.(Zigzag.inverse.(i)) <- coefs.(i) * qnt_tab.(i)
  done;
  dc
;;

let decode_coefficient_block
    ~bits
    ~coefs
    ~dequant
    ~dc_pred
    ~(dc_tab : Tables.dc Tables.Lut.t)
    ~(ac_tab : Tables.ac Tables.Lut.t)
    ~qnt_tab
  =
  clear_block coefs;
  (* Huffman decode and dequantize ac coefficients. *)
  huffman_decode ~bits ~coefs ~dc_tab ~ac_tab;
  (* grab the updated dc predictor and dequantize the coefficient. *)
  dequantize_dc_pred_and_inverse_zigzag ~qnt_tab ~dc_pred ~coefs ~dequant
;;

module Component = struct
  type t =
    { plane : (Plane.t[@sexp.opaque])
    ; decoded_width : int
    ; decoded_height : int
    ; actual_width : int
    ; actual_height : int
    ; mutable x : int
    ; mutable y : int
    ; mutable dc_pred : int
    ; component : Markers.Component.t
    ; scan : Markers.Scan_component.t
    ; quant_table : int array
    ; dc_tab : (Tables.dc Tables.Lut.t[@sexp.opaque])
    ; ac_tab : (Tables.ac Tables.Lut.t[@sexp.opaque])
    ; coefs : int array
    ; dequant : int array
    ; idct : int array
    ; recon : int array
    }
  [@@deriving sexp_of, fields]

  module Summary = struct
    type nonrec t = t

    let sexp_of_t { x; y; dc_pred; component; coefs; dequant; idct; recon; _ } =
      [%message
        (x : int)
          (y : int)
          (dc_pred : int)
          (component.identifier : int)
          (coefs : Util.coef_block)
          (dequant : Util.coef_block)
          (idct : Util.pixel_block)
          (recon : Util.pixel_block)]
    ;;
  end
end

type t =
  { header : Header.t
  ; components : Component.t array
  ; entropy_coded_bits : Bits.t
  }
[@@deriving fields]

let clip x = if x < -128 then -128 else if x > 127 then 127 else x

let recon { Component.plane; x; y; idct; recon; _ } =
  for j = 0 to 7 do
    for i = 0 to 7 do
      let k = i + (j * 8) in
      idct.(k) <- clip idct.(k);
      recon.(k) <- idct.(k) + 128;
      Plane.(plane.![x + i, y + j] <- Char.of_int_exn recon.(k))
    done
  done
;;

let find_component (scan : Markers.Scan_component.t) (frame : Markers.Sof.t) =
  match Array.find frame.components ~f:(fun c -> c.identifier = scan.selector) with
  | None -> raise_s [%message "unable to find component identifier"]
  | Some c -> c
;;

let find_quant_table (quant_tables : Markers.Dqt.t list) id =
  match List.find quant_tables ~f:(fun qnt -> qnt.table_identifier = id) with
  | None -> raise_s [%message "unable to find quantisation table"]
  | Some q -> q.elements
;;

let find_huffman_table ac_dc (huffman_tables : Markers.Dht.t list) id =
  match
    List.find huffman_tables ~f:(fun huff ->
        huff.table_class = ac_dc && huff.destination_identifier = id)
  with
  | None -> raise_s [%message "unable to find huffman table"]
  | Some huff -> huff
;;

let find_dc_huffman_table huffman_tables id =
  let table = find_huffman_table 0 huffman_tables id in
  Tables.Specification.create_dc_code_table
    { Tables.Specification.lengths = table.lengths; values = table.values }
  |> Tables.Lut.create
;;

let find_ac_huffman_table huffman_tables id =
  let table = find_huffman_table 1 huffman_tables id in
  Tables.Specification.create_ac_code_table
    { Tables.Specification.lengths = table.lengths; values = table.values }
  |> Tables.Lut.create
;;

let extract_entropy_coded_bits bits =
  let buffer = Buffer.create 1024 in
  let rec search_for_marker pos prev =
    let c = Bits.get_byte bits pos in
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
      search_for_marker (pos + 1) c)
  in
  let pos = Bits.bit_pos bits lsr 3 in
  let entropy_data = search_for_marker pos '\x00' in
  Bits.create entropy_data
;;

let frame_and_scan (header : Header.t) =
  match header with
  | { frame = Some frame
    ; scan = Some scan
    ; quant_tables = _
    ; restart_interval = _
    ; huffman_tables = _
    } -> frame, scan
  | _ -> raise_s [%message "From start of frame or start of scan marker"]
;;

let max_component_scale (frame : Markers.Sof.t) =
  let max_hscale =
    Array.fold frame.components ~init:0 ~f:(fun m c -> max m c.horizontal_sampling_factor)
  in
  let max_vscale =
    Array.fold frame.components ~init:0 ~f:(fun m c -> max m c.vertical_sampling_factor)
  in
  max_hscale, max_vscale
;;

let init (header : Header.t) bits =
  let frame, scan = frame_and_scan header in
  let max_hscale, max_vscale = max_component_scale frame in
  let rounded_width = Int.round_up ~to_multiple_of:(max_hscale * 8) frame.width in
  let rounded_height = Int.round_up ~to_multiple_of:(max_vscale * 8) frame.height in
  let components =
    Array.map scan.scan_components ~f:(fun scan ->
        let component = find_component scan frame in
        let decoded_width =
          rounded_width * component.horizontal_sampling_factor / max_hscale
        in
        let decoded_height =
          rounded_height * component.vertical_sampling_factor / max_vscale
        in
        let actual_width =
          rounded_width * component.horizontal_sampling_factor / max_hscale
        in
        let actual_height =
          rounded_height * component.vertical_sampling_factor / max_vscale
        in
        { Component.plane = Plane.create ~width:decoded_width ~height:decoded_height
        ; decoded_width
        ; decoded_height
        ; actual_width
        ; actual_height
        ; dc_pred = 0
        ; x = 0
        ; y = 0
        ; component
        ; scan
        ; quant_table =
            find_quant_table header.quant_tables component.quantization_table_identifier
        ; dc_tab = find_dc_huffman_table header.huffman_tables scan.dc_coef_selector
        ; ac_tab = find_ac_huffman_table header.huffman_tables scan.ac_coef_selector
        ; coefs = Array.init 64 ~f:(Fn.const 0)
        ; dequant = Array.init 64 ~f:(Fn.const 0)
        ; idct = Array.init 64 ~f:(Fn.const 0)
        ; recon = Array.init 64 ~f:(Fn.const 0)
        })
  in
  { header; components; entropy_coded_bits = extract_entropy_coded_bits bits }
;;

let decode_block ~bits ~(component : Component.t) =
  component.dc_pred
    <- decode_coefficient_block
         ~bits
         ~coefs:component.coefs
         ~dequant:component.dequant
         ~dc_pred:component.dc_pred
         ~dc_tab:component.dc_tab
         ~ac_tab:component.ac_tab
         ~qnt_tab:component.quant_table;
  Array.blito ~src:component.dequant ~dst:component.idct ();
  Dct.Chen.inverse_8x8 component.idct;
  recon component
;;

let decode_component_seq ~bits ~(component : Component.t) ~blk_x ~blk_y =
  let vscale = component.component.vertical_sampling_factor in
  let hscale = component.component.horizontal_sampling_factor in
  Sequence.init vscale ~f:(fun y ->
      Sequence.init hscale ~f:(fun x ->
          component.x <- ((blk_x * vscale) + x) * 8;
          component.y <- ((blk_y * hscale) + y) * 8;
          decode_block ~bits ~(component : Component.t);
          component))
  |> Sequence.concat
;;

let decode_seq { header = _; components; entropy_coded_bits } =
  (* each component will be the same number of 'macro' blocks in size, so it doesn't matter
     which we use. *)
  let macroblocks_wide =
    components.(0).decoded_width
    / (8 * components.(0).component.horizontal_sampling_factor)
  in
  let macroblocks_high =
    components.(0).decoded_height / (8 * components.(0).component.vertical_sampling_factor)
  in
  Sequence.init macroblocks_high ~f:(fun blk_y ->
      Sequence.init macroblocks_wide ~f:(fun blk_x ->
          Sequence.init (Array.length components) ~f:(fun id ->
              decode_component_seq
                ~bits:entropy_coded_bits
                ~component:components.(id)
                ~blk_x
                ~blk_y)
          |> Sequence.concat)
      |> Sequence.concat)
  |> Sequence.concat
;;

let decode decoder = decode_seq decoder |> Sequence.iter ~f:(fun _ -> ())

let get_decoded_planes decoder =
  Array.map decoder.components ~f:(fun component -> component.plane)
;;

let crop (component : Component.t) =
  if component.decoded_width <> component.actual_width
     || component.decoded_height <> component.actual_height
  then (
    let plane =
      Plane.create ~width:component.actual_width ~height:component.actual_height
    in
    for row = 0 to component.actual_height - 1 do
      for col = 0 to component.actual_width - 1 do
        Plane.(plane.![col, row] <- component.plane.![col, row])
      done
    done;
    plane)
  else component.plane
;;

let get_yuv_frame decoder =
  let planes = Array.map decoder.components ~f:(fun component -> crop component) in
  Frame.of_planes ~y:planes.(0) ~u:planes.(1) ~v:planes.(2)
;;

let decode_a_frame bits =
  let header = Header.decode bits in
  let decoder = init header bits in
  decode decoder;
  get_yuv_frame decoder
;;

module For_testing = struct
  let mag = mag'
  let extract_entropy_coded_bits = extract_entropy_coded_bits

  module Sequenced = struct
    let decode decoder = Sequence.memoize (decode_seq decoder)
  end
end
