open! Base
open! Hardcaml
open Signal

module type Config = sig
  val input_bits : int
  val output_bits : int
  val rom_prec : int
  val transpose_prec : int
  val transform_matrix : float Hardcaml_jpeg_model.Dct.Matrix8x8.t
end

module Dct_config = struct
  let input_bits = 8
  let output_bits = 12
  let rom_prec = 12
  let transpose_prec = 4

  let transform_matrix =
    Hardcaml_jpeg_model.Dct.Floating_point.Eight_point.forward_transform_matrix
  ;;
end

module Idct_config = struct
  let input_bits = 12
  let output_bits = 8
  let rom_prec = 12
  let transpose_prec = 4

  let transform_matrix =
    Hardcaml_jpeg_model.Dct.Floating_point.Eight_point.inverse_transform_matrix
  ;;
end

module Make (Config : Config) = struct
  open Config

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
      ; pixel_write : 'a
      ; transpose_coef_out : 'a [@bits transpose_bits]
      ; transpose_write : 'a
      ; coef_read : 'a
      ; transpose_read : 'a
      ; read_address : 'a [@bits 6]
      ; write_address : 'a [@bits 6]
      ; done_ : 'a
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

  let coef_rom ~row ~col =
    let open Hardcaml_jpeg_model.Dct in
    let rnd f =
      Float.(f * (2. ** Float.of_int rom_prec) |> Float.round_nearest |> Float.to_int)
    in
    transform_matrix
    |> Matrix8x8.map ~f:(fun x -> Signal.of_int ~width:rom_prec (rnd x))
    |> Array.map ~f:(fun d -> mux col (Array.to_list d))
    |> Array.to_list
    |> mux row
  ;;

  let create scope (i : _ I.t) =
    let ( -- ) = Scope.naming scope in
    let ( --. ) s n = ignore (s -- n : Signal.t) in
    let reg ?enable d = Clocking.reg i.clocking ?enable d in
    let pipeline ?enable ~n d = Clocking.pipeline i.clocking ?enable ~n d in
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
      let row = mux2 pass.value y.value x.value in
      let col = mux2 pass.value z.value z.value in
      reg (coef_rom ~row ~col) -- "dct_coef"
    in
    let mul =
      let coef =
        mux2 (reg pass.value) i.transpose_coef_in (sresize i.coef transpose_bits)
      in
      reg (coef *+ dct_coef) -- "dct_mul"
    in
    let mac =
      reg_fb (Clocking.to_spec i.clocking) ~enable:vdd ~width:mac_bits ~f:(fun d ->
          let mul = sresize mul mac_bits in
          mux2 (pipeline ~n:2 z.value ==:. 0) mul (d +: mul))
      -- "dct_mac"
    in
    let module Fixed = Hardcaml_fixed_point.Signed (Signal) in
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
    let clipped_and_rounded_pixel =
      let mac = Fixed.create (rom_prec + transpose_prec) mac in
      Fixed.(
        signal
          (resize
             ~round:Fixed.Round.tie_away_from_zero
             ~overflow:Fixed.Overflow.saturate
             mac
             output_bits
             0))
      (* |> Clocking.reg i.clocking *)
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
    let read_address = mux2 pass.value (x.value @: z.value) (z.value @: y.value) in
    let read = sm.is Run in
    let write =
      let writing = z.value ==:. 7 in
      pipeline ~n:3 writing
    in
    let write_pass = pipeline ~n:3 pass.value in
    let write_address = pipeline ~n:3 (x.value @: y.value) in
    { O.pixel = clipped_and_rounded_pixel
    ; pixel_write = write &: write_pass
    ; transpose_coef_out = rounded_transpose_coef
    ; transpose_write = write &: ~:write_pass
    ; coef_read = read &: ~:(pass.value)
    ; transpose_read = read &: pass.value
    ; read_address
    ; write_address
    ; done_ = sm.is Start
    }
  ;;

  let hierarchical scope =
    let module Hier = Hierarchy.In_scope (I) (O) in
    Hier.hierarchical ~scope ~name:"dct" create
  ;;
end
