open! Base
module Bits = Bitstream_reader.Make (String)

module Header = struct
  type t =
    { frame : Markers.Sof.t option
    ; scan : Markers.Sos.t option
    ; quant_tables : Markers.Dqt.t list
    ; restart_interval : Markers.Dri.t option
    ; huffman_tables : Markers.Dht.t list
    }
  [@@deriving sexp_of]

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
    Stdio.eprint_s [%message (marker_code : Int.Hex.t)];
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
end

let create_code_table lengths values =
  let rec build code length_pos =
    if length_pos = Array.length lengths
    then []
    else if lengths.(length_pos) = 0
    then build (code lsl 1) (length_pos + 1)
    else
      List.init lengths.(length_pos) ~f:(fun i ->
          { Tables.length = length_pos + 1
          ; bits = code + i
          ; data = values.(length_pos).(i)
          })
      :: build ((code + lengths.(length_pos)) lsl 1) (length_pos + 1)
  in
  build 0 0 |> List.concat
;;

let mag bits cat =
  if cat = 0
  then 0
  else (
    let v = Bits.get bits cat in
    if v land (1 lsl (cat - 1)) <> 0
    then (* +ve coeff *)
      v
    else (* -ve coeff *)
      (v lor (-1 lsl cat)) + 1)
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

let huffman_decode
    ~bits
    ~block
    ~dc_pred
    ~(dc_tab : Tables.dc Tables.Lut.t)
    ~(ac_tab : Tables.ac Tables.Lut.t)
    ~qnt_tab
  =
  let izigzag = Zigzag.inverse in
  (* clear block *)
  for i = 0 to 63 do
    block.(i) <- 0
  done;
  (* dc coefficient *)
  let dc = dc_code bits dc_tab in
  let dc_pred = dc_pred + dc in
  block.(0) <- dc_pred * qnt_tab.(0);
  (* ac coefficients *)
  let cof_cnt = ref 1 in
  while !cof_cnt < 64 do
    let mag, run = ac_code bits ac_tab in
    if mag = 0 && run = 0 then cof_cnt := 63 else cof_cnt := !cof_cnt + run;
    if !cof_cnt >= 64
    then raise_s [%message "coefficient index out of range:" (!cof_cnt : int)];
    block.(izigzag.(!cof_cnt)) <- mag * qnt_tab.(!cof_cnt);
    cof_cnt := !cof_cnt + 1
  done;
  (* Stdio.print_s [%message (block : block)]; *)
  dc_pred
;;

let clip x = if x < -128 then -128 else if x > 127 then 127 else x
let level_shift x = Char.of_int_exn (clip x + 128)

let recon ~block ~plane x y =
  for j = 0 to 7 do
    for i = 0 to 7 do
      Plane.(plane.![x + i, y + j] <- level_shift block.(i + (j * 8)))
    done
  done
;;

module Component = struct
  type t =
    { plane : (Plane.t[@sexp.opaque])
    ; width : int
    ; height : int
    ; mutable x : int
    ; mutable y : int
    ; mutable dc_pred : int
    ; component : Markers.Component.t
    ; scan : Markers.Scan_component.t
    ; quant_table : int array
    ; dc_tab : (Tables.dc Tables.Lut.t[@sexp.opaque])
    ; ac_tab : (Tables.ac Tables.Lut.t[@sexp.opaque])
    }
  [@@deriving sexp_of]
end

type t =
  { components : Component.t array
  ; block : int array
  }

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
  | Some huff -> create_code_table huff.lengths huff.values
;;

let find_dc_huffman_table huffman_tables id =
  find_huffman_table 0 huffman_tables id |> Tables.Lut.create
;;

let find_ac_huffman_table huffman_tables id =
  let to_ac_run_size x =
    let { Tables.length; bits; data = rrrrssss } = x in
    { Tables.length
    ; bits
    ; data = { Tables.run = rrrrssss lsr 4; size = rrrrssss land 0xf }
    }
  in
  find_huffman_table 1 huffman_tables id
  |> List.map ~f:to_ac_run_size
  |> Tables.Lut.create
;;

let init (header : Header.t) =
  match header with
  | { frame = Some frame
    ; scan = Some scan
    ; quant_tables
    ; restart_interval = _
    ; huffman_tables = _
    } ->
    let max_hscale =
      Array.fold frame.components ~init:0 ~f:(fun m c ->
          max m c.horizontal_sampling_factor)
    in
    let max_vscale =
      Array.fold frame.components ~init:0 ~f:(fun m c -> max m c.vertical_sampling_factor)
    in
    let components =
      Array.map scan.scan_components ~f:(fun scan ->
          let component = find_component scan frame in
          let width = frame.width * component.horizontal_sampling_factor / max_hscale in
          let height = frame.height * component.vertical_sampling_factor / max_vscale in
          { Component.plane = Plane.create ~width ~height
          ; width
          ; height
          ; dc_pred = 0
          ; x = 0
          ; y = 0
          ; component
          ; scan
          ; quant_table =
              find_quant_table quant_tables component.quantization_table_identifier
          ; dc_tab = find_dc_huffman_table header.huffman_tables scan.dc_coef_selector
          ; ac_tab = find_ac_huffman_table header.huffman_tables scan.ac_coef_selector
          })
    in
    { components; block = Array.init 64 ~f:(Fn.const 0) }
  | _ -> raise_s [%message "not start of frame or sequence" (header : Header.t)]
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

let decode_component ~bits ~decoder ~(component : Component.t) ~blk_x ~blk_y =
  let vscale = component.component.vertical_sampling_factor in
  let hscale = component.component.horizontal_sampling_factor in
  for y = 0 to vscale - 1 do
    for x = 0 to hscale - 1 do
      let x_pos = ((blk_x * vscale) + x) * 8 in
      let y_pos = ((blk_y * hscale) + y) * 8 in
      Stdio.eprint_s
        [%message
          (blk_x : int) (blk_y : int) (x : int) (y : int) (x_pos : int) (y_pos : int)];
      component.dc_pred
        <- huffman_decode
             ~bits
             ~block:decoder.block
             ~dc_pred:component.dc_pred
             ~dc_tab:component.dc_tab
             ~ac_tab:component.ac_tab
             ~qnt_tab:component.quant_table;
      Dct.Chen.inverse_8x8 decoder.block;
      (* Stdio.print_s [%message (decoder.block : block)]; *)
      (* let () = assert false in *)
      recon ~block:decoder.block ~plane:component.plane x_pos y_pos
    done
  done
;;

let decode_frame (bits : Bits.t) =
  let header = Header.decode bits Header.empty in
  (* the following shows we deocde the tables correctly, and they match the
     default tables used ~everywhere it seems *)
  (* List.iter header.huffman_tables ~f:(fun h ->
   *     Stdio.print_s
   *       [%message (create_code_table h.lengths h.values : int Tables.coef list)]);
   * Stdio.print_s
   *   [%message
   *     (Tables.dc_luma : Tables.dc_coef list)
   *       (Tables.ac_luma : Tables.ac_coef list)
   *       (Tables.dc_chroma : Tables.dc_coef list)
   *       (Tables.ac_chroma : Tables.ac_coef list)];
   * assert (
   *   Poly.equal
   *     (create_code_table
   *        (List.nth_exn header.huffman_tables 3).lengths
   *        (List.nth_exn header.huffman_tables 3).values)
   *     Tables.dc_luma);
   * assert (
   *   Poly.equal
   *     (create_code_table
   *        (List.nth_exn header.huffman_tables 1).lengths
   *        (List.nth_exn header.huffman_tables 1).values)
   *     Tables.dc_chroma);
   * assert (
   *   Poly.equal
   *     (create_code_table
   *        (List.nth_exn header.huffman_tables 2).lengths
   *        (List.nth_exn header.huffman_tables 2).values)
   *     (List.map Tables.ac_luma ~f:(fun h ->
   *          { h with data = (h.data.run lsl 4) lor h.data.size })));
   * assert (
   *   Poly.equal
   *     (create_code_table
   *        (List.nth_exn header.huffman_tables 0).lengths
   *        (List.nth_exn header.huffman_tables 0).values)
   *     (List.map Tables.ac_chroma ~f:(fun h ->
   *          { h with data = (h.data.run lsl 4) lor h.data.size }))); *)
  (* XXXXX delete the test code above. Or maybe pull it into an expect test or somesuch. *)
  let decoder = init header in
  let entropy_coded_bits = extract_entropy_coded_bits bits in
  let blocks_wide =
    decoder.components.(0).width
    / (8 * decoder.components.(0).component.horizontal_sampling_factor)
  in
  let blocks_high =
    decoder.components.(0).height
    / (8 * decoder.components.(0).component.vertical_sampling_factor)
  in
  for blk_y = 0 to blocks_high - 1 do
    for blk_x = 0 to blocks_wide - 1 do
      for id = 0 to Array.length decoder.components - 1 do
        decode_component
          ~bits:entropy_coded_bits
          ~decoder
          ~component:decoder.components.(id)
          ~blk_x
          ~blk_y
      done
    done
  done;
  Array.map decoder.components ~f:(fun c -> c.plane)
;;
