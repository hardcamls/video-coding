open! Base
open! Hardcaml
open! Signal

let dct_coef_bits = 12
let quant_coef_bits = 13
let log_num_quant_tables = 1
let num_quant_tables = 1 lsl log_num_quant_tables
let pipeline_depth = 4
let one_over_quant_coef q = (1 lsl (quant_coef_bits - 1)) / q

module Quant_write = struct
  type 'a t =
    { quant : 'a [@bits quant_coef_bits]
    ; write : 'a
    ; address : 'a [@bits 6 + log_num_quant_tables]
    }
  [@@deriving sexp_of, hardcaml]
end

module I = struct
  type 'a t =
    { clocking : 'a Clocking.t
    ; enable : 'a
    ; table_select : 'a [@bits log_num_quant_tables]
    ; dct_coef : 'a [@bits dct_coef_bits]
    ; dct_coef_read : 'a
    ; dct_coef_address : 'a [@bits 6]
    ; quant : 'a Quant_write.t [@rtlprefix "wr$"]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { quant_coef : 'a [@bits dct_coef_bits]
    ; quant_coef_write : 'a
    ; quant_coef_address : 'a [@bits 6]
    }
  [@@deriving sexp_of, hardcaml]
end

let multiply (type a) (module Signal : Comb.S with type t = a) (d : a) q =
  Signal.(d *+ ue q)
;;

let round (type a) (module Signal : Comb.S with type t = a) (doq : a) =
  let open Signal in
  let half = of_int ~width:(width doq) (1 lsl (quant_coef_bits - 2)) in
  let half_minus_1 =
    Signal.of_int ~width:(width doq) ((1 lsl (quant_coef_bits - 2)) - 1)
  in
  let rnd = mux2 (msb doq) half_minus_1 half in
  lsbs (lsbs (drop_bottom (doq +: rnd) (quant_coef_bits - 1)))
;;

let create _scope (i : _ I.t) =
  let qram =
    Ram.create
      ~size:(64 * num_quant_tables)
      ~collision_mode:Write_before_read
      ~write_ports:
        [| { write_clock = i.clocking.clock
           ; write_data = i.quant.quant
           ; write_address = i.quant.address
           ; write_enable = i.quant.write
           }
        |]
      ~read_ports:
        [| { read_clock = i.clocking.clock
           ; read_address = i.table_select @: i.dct_coef_address
           ; read_enable = i.dct_coef_read &: i.enable
           }
        |]
      ()
  in
  let reg, pipe =
    ( reg (Clocking.to_spec_no_clear i.clocking) ~enable:i.enable
    , pipeline (Clocking.to_spec_no_clear i.clocking) ~enable:i.enable )
  in
  (* 4 cycles: 2 to look up coefficient, 1 for multiply, 1 for rounding. *)
  let q = reg qram.(0) in
  let d = reg i.dct_coef in
  let doq = reg (multiply (module Signal) d q) in
  let quant_coef = reg (round (module Signal) doq) in
  assert (width quant_coef = dct_coef_bits);
  { O.quant_coef
  ; quant_coef_address = pipe ~n:4 i.dct_coef_address
  ; quant_coef_write = pipe ~n:4 i.dct_coef_read
  }
;;

let hierarchical scope =
  let module Hier = Hierarchy.In_scope (I) (O) in
  Hier.hierarchical ~scope ~name:"qnt" create
;;
