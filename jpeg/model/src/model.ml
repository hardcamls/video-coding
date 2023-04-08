open! Base
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

let create_code_table lengths (values : int array array) =
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

let mag' cat code =
  if code land (1 lsl (cat - 1)) <> 0
  then (* +codee coeff *)
    code
  else (* -codee coeff *)
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
    ; coefs : int array
    ; dequant : int array
    ; idct : int array
    }
  [@@deriving sexp_of]

  module Summary = struct
    type nonrec t = t

    let sexp_of_block len (t : int array) =
      let sexp_of_hex x =
        let hex x =
          let x = x land 15 in
          if x < 10
          then Char.of_int_exn (x + Char.to_int '0')
          else Char.of_int_exn (x - 10 + Char.to_int 'a')
        in
        sexp_of_string
          (String.init len ~f:(fun i ->
               let i = len - i - 1 in
               hex (x lsr (i * 4))))
      in
      let block = Array.init 8 ~f:(fun y -> Array.init 8 ~f:(fun x -> t.(x + (y * 8)))) in
      [%sexp_of: hex array array] block
    ;;

    let sexp_of_t { x; y; dc_pred; component; coefs; dequant; idct; _ } =
      let sexp_of_coef = sexp_of_block 3 in
      let sexp_of_pel = sexp_of_block 2 in
      [%message
        (x : int)
          (y : int)
          (dc_pred : int)
          (component.identifier : int)
          (coefs : coef)
          (dequant : coef)
          (idct : pel)]
    ;;
  end
end

type t = { components : Component.t array }

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
          ; coefs = Array.init 64 ~f:(Fn.const 0)
          ; dequant = Array.init 64 ~f:(Fn.const 0)
          ; idct = Array.init 64 ~f:(Fn.const 0)
          })
    in
    { components }
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
  recon ~block:component.idct ~plane:component.plane component.x component.y
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

let decode_seq decoder (bits : Bits.t) =
  let entropy_coded_bits = extract_entropy_coded_bits bits in
  let blocks_wide =
    decoder.components.(0).width
    / (8 * decoder.components.(0).component.horizontal_sampling_factor)
  in
  let blocks_high =
    decoder.components.(0).height
    / (8 * decoder.components.(0).component.vertical_sampling_factor)
  in
  Sequence.init blocks_high ~f:(fun blk_y ->
      Sequence.init blocks_wide ~f:(fun blk_x ->
          Sequence.init (Array.length decoder.components) ~f:(fun id ->
              decode_component_seq
                ~bits:entropy_coded_bits
                ~component:decoder.components.(id)
                ~blk_x
                ~blk_y)
          |> Sequence.concat)
      |> Sequence.concat)
  |> Sequence.concat
;;

let decode decoder bits = decode_seq decoder bits |> Sequence.iter ~f:(fun _ -> ())

let get_frame decoder =
  Frame.of_planes
    ~y:decoder.components.(0).plane
    ~u:decoder.components.(1).plane
    ~v:decoder.components.(2).plane
;;

let decode_a_frame bits =
  let header = Header.decode bits in
  let decoder = init header in
  decode decoder bits;
  get_frame decoder
;;

module For_testing = struct
  let mag = mag'
  let extract_entropy_coded_bits = extract_entropy_coded_bits

  module Sequenced = struct
    let decode decoder bits = Sequence.memoize (decode_seq decoder bits)
  end
end
