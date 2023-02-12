open! Base
open! Hardcaml
open Signal

let input_bits = 12
let pixel_bits = 8
let rom_bits = 12

module I = struct
  type 'a t =
    { clocking : 'a Clocking.t
    ; start : 'a
    ; coef : 'a [@bits 12]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { pixel : 'a [@bits pixel_bits]
    ; pixel_address : 'a [@bits 6]
    ; pixel_write : 'a
    ; coef_address : 'a [@bits 6]
    ; coef_read : 'a
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
    Float.(f * (2. ** Float.of_int rom_bits) |> Float.round_nearest |> Float.to_int)
  in
  Eight_point.inverse_transform_matrix
  |> Util.map ~f:(fun x -> Signal.of_int ~width:rom_bits (rnd x))
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
  let dct_coef = idct_rom ~row:y.value ~col:z.value -- "dct_coef" in
  let mul = Clocking.reg i.clocking (i.coef *+ dct_coef) -- "dct_mul" in
  let mac =
    let mac_bits = input_bits + rom_bits + 3 in
    reg_fb (Clocking.to_spec i.clocking) ~enable:vdd ~width:mac_bits ~f:(fun d ->
        let mul = sresize mul mac_bits in
        mux2 (Clocking.reg i.clocking (z.value ==:. 0)) mul (d +: mul))
    -- "dct_mac"
  in
  let clipped_and_rounded_pixel =
    let module Fixed = Hardcaml_fixed_point.Signed (Signal) in
    let mac = Fixed.create rom_bits mac in
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
  let coef_address = mux2 pass.value (z.value @: x.value) (x.value @: z.value) in
  let read = sm.is Run in
  let write =
    let writing = z.value ==:. 7 in
    pipeline (Clocking.to_spec i.clocking) ~n:3 writing
  in
  { (O.Of_signal.of_int 0) with
    pixel = clipped_and_rounded_pixel
  ; pixel_write = write
  ; coef_address
  ; coef_read = read
  }
;;
