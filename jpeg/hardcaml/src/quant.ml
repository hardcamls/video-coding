open! Base
open! Hardcaml
open! Signal

let dct_coef_bits = 12
let quant_coef_bits = 12
let log_num_quant_tables = 1
let num_quant_tables = 1 lsl log_num_quant_tables
let pipeline_depth = 4
let one_over_quant_coef q = (1 lsl quant_coef_bits) / q

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
    ; dct_coef_write : 'a
    ; dct_coef_address : 'a [@bits 6]
    ; quant : 'a Quant_write.t
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
           ; read_enable = i.dct_coef_write &: i.enable
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
  let d = pipe ~n:2 i.dct_coef in
  let half = Signal.of_int ~width:(width d + width q) (1 lsl (quant_coef_bits - 1)) in
  let half_minus_1 =
    Signal.of_int ~width:(width d + width q) ((1 lsl (quant_coef_bits - 1)) - 1)
  in
  let doq = reg (d *+ q) in
  let rnd = mux2 (msb doq) half_minus_1 half in
  let quant_coef = reg (drop_bottom (doq +: rnd) quant_coef_bits) in
  { O.quant_coef
  ; quant_coef_address = pipe ~n:4 i.dct_coef_address
  ; quant_coef_write = pipe ~n:4 i.dct_coef_write
  }
;;

let hierarchical scope =
  let module Hier = Hierarchy.In_scope (I) (O) in
  Hier.hierarchical ~scope ~name:"qnt" create
;;
