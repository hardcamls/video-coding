open Base
open Hardcaml
open Signal

module I = struct
  type 'a t =
    { clocking : 'a Clocking.t
    ; coefs_in : 'a Codeblock_decoder.Idct_coefs.t [@rtlprefix "i_"]
    ; dqt : 'a Markers.Dqt.Fields.t
    ; table_select : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t = { coefs_out : 'a Codeblock_decoder.Idct_coefs.t [@rtlprefix "o_"] }
  [@@deriving sexp_of, hardcaml]
end

let create _scope (i : _ I.t) =
  let q =
    List.init 2 ~f:(fun id ->
        (Ram.create
           ~size:64
           ~collision_mode:Write_before_read
           ~write_ports:
             [| { write_clock = i.clocking.clock
                ; write_address = i.dqt.element_address
                ; write_data = i.dqt.element.:[7, 0]
                ; write_enable =
                    i.dqt.element_write &: (i.dqt.fields.table_identifier ==:. id)
                }
             |]
           ~read_ports:
             [| { read_clock = i.clocking.clock
                ; read_address = i.coefs_in.address
                ; read_enable = i.coefs_in.write
                }
             |]
           ()).(0))
    |> mux i.table_select
  in
  let zz_addr =
    Hardcaml_jpeg_model.Zigzag.inverse
    |> Array.to_list
    |> List.map ~f:(Signal.of_int ~width:6)
    |> mux (Clocking.reg i.clocking i.coefs_in.address)
  in
  let dequantized_coef = (ue q *+ Clocking.reg i.clocking i.coefs_in.coef).:[11, 0] in
  { O.coefs_out =
      { coef = dequantized_coef
      ; address = zz_addr
      ; write = Clocking.reg i.clocking i.coefs_in.write
      }
  }
;;

let hierarchical scope =
  let module Hier = Hierarchy.In_scope (I) (O) in
  Hier.hierarchical ~scope ~name:"dqnt" create
;;
