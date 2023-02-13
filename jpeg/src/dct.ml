open! Base
open! Hardcaml
open Signal

let input_bits = 12
let output_bits = 8
let rom_prec = 12
let transpose_prec = 4
let transpose_bits = input_bits + transpose_prec + 3
let mac_bits = transpose_bits + rom_prec + 3

module I = struct
  type 'a t =
    { clocking : 'a Clocking.t
    ; start : 'a
    ; coef : 'a [@bits input_bits]
    ; transpose_coef_in : 'a [@bits transpose_bits]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { pixel : 'a [@bits output_bits]
    ; pixel_address : 'a [@bits 6]
    ; pixel_write : 'a
    ; transpose_coef_out : 'a [@bits transpose_bits]
    ; transpose_write_address : 'a [@bits 6]
    ; transpose_write : 'a
    ; coef_address : 'a [@bits 6]
    ; coef_read : 'a
    ; transpose_read_address : 'a [@bits 6]
    ; transpose_read : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module State = struct
  type t =
    | Start
    | Run
  [@@deriving sexp_of, compare, enumerate]
end

module Var = Always.Variable

let idct_rom ~row ~col =
  let open Hardcaml_jpeg_model.Dct.Reference in
  let rnd f =
    Float.(f * (2. ** Float.of_int rom_prec) |> Float.round_nearest |> Float.to_int)
  in
  Eight_point.inverse_transform_matrix
  |> Util.map ~f:(fun x -> Signal.of_int ~width:rom_prec (rnd x))
  |> Array.map ~f:(fun d -> mux col (Array.to_list d))
  |> Array.to_list
  |> mux row
;;

let create scope (i : _ I.t) =
  let ( -- ) = Scope.naming scope in
  let ( --. ) s n = ignore (s -- n : Signal.t) in
  let sm = Always.State_machine.create (module State) (Clocking.to_spec i.clocking) in
  sm.current --. "STATE";
  let pass = Clocking.Var.reg i.clocking ~width:1 in
  pass.value --. "pass";
  let x = Clocking.Var.reg i.clocking ~width:3 in
  x.value --. "x";
  let y = Clocking.Var.reg i.clocking ~width:3 in
  y.value --. "y";
  let z = Clocking.Var.reg i.clocking ~width:3 in
  z.value --. "z";
  let dct_coef =
    let row = mux2 pass.value z.value y.value in
    let col = mux2 pass.value y.value z.value in
    idct_rom ~row ~col -- "dct_coef"
  in
  let mul =
    (* XXX coef's should be delayed by 1 cycle, but they aren't in the testbench right now. *)
    let coef = mux2 pass.value i.transpose_coef_in (uresize i.coef transpose_bits) in
    Clocking.reg i.clocking (coef *+ dct_coef) -- "dct_mul"
  in
  let mac =
    reg_fb (Clocking.to_spec i.clocking) ~enable:vdd ~width:mac_bits ~f:(fun d ->
        let mul = sresize mul mac_bits in
        mux2 (Clocking.reg i.clocking (z.value ==:. 0)) mul (d +: mul))
    -- "dct_mac"
  in
  let module Fixed = Hardcaml_fixed_point.Signed (Signal) in
  let clipped_and_rounded_pixel =
    let mac = Fixed.create rom_prec mac in
    Fixed.(
      signal
        (resize
           ~round:Fixed.Round.tie_away_from_zero
           ~overflow:Fixed.Overflow.saturate
           mac
           8
           0))
    |> Clocking.reg i.clocking
  in
  let rounded_transpose_coef =
    let mac = Fixed.create rom_prec mac in
    Fixed.(
      signal
        (resize
           ~round:Fixed.Round.tie_away_from_zero
           ~overflow:Fixed.Overflow.wrap
           mac
           (input_bits + 3)
           transpose_prec))
  in
  assert (width rounded_transpose_coef = transpose_bits);
  Always.(
    compile
      [ sm.switch
          [ ( Start
            , [ when_
                  i.start
                  [ pass <--. 0; x <--. 0; y <--. 0; z <--. 0; sm.set_next Run ]
              ] )
          ; ( Run
            , [ z <-- z.value +:. 1
              ; when_
                  (z.value ==:. 7)
                  [ z <--. 0
                  ; y <-- y.value +:. 1
                  ; when_
                      (y.value ==:. 7)
                      [ y <--. 0
                      ; x <-- x.value +:. 1
                      ; when_
                          (x.value ==:. 7)
                          [ x <--. 0
                          ; pass <-- ~:(pass.value)
                          ; when_ pass.value [ sm.set_next Start ]
                          ]
                      ]
                  ]
              ] )
          ]
      ]);
  let coef_address = mux2 pass.value (x.value @: z.value) (z.value @: x.value) in
  let read = sm.is Run in
  let write =
    let writing = z.value ==:. 7 in
    pipeline (Clocking.to_spec i.clocking) ~n:3 writing
  in
  let write_pass = pipeline (Clocking.to_spec i.clocking) ~n:3 pass.value in
  let write_address = pipeline (Clocking.to_spec i.clocking) ~n:3 coef_address in
  { O.pixel = clipped_and_rounded_pixel
  ; pixel_address = write_address
  ; pixel_write = write &: write_pass
  ; transpose_coef_out = rounded_transpose_coef
  ; transpose_write_address = write_address
  ; transpose_write = write &: ~:write_pass
  ; coef_address
  ; coef_read = read &: ~:(pass.value)
  ; transpose_read_address = coef_address
  ; transpose_read = read &: pass.value
  }
;;
