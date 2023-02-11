open! Base
open! Hardcaml
open Signal

(* There are a few architectures we can play with here.

   All are based on an initial decomposition of a 1d-8pt IDCT into 2 4pt IDCTs
   followed by a butterfly step.

   The first set of architectures are all multiplier based - we will build
   versions using 1, 4 and 16 multipliers. As we build for higher throughput,
   buffering requirements will become more difficult to manage.

   Finally, we can look at an interesting bit serial arithmetic based design
   using Rom Accumulators. *)

(* The coefficients are in the range (-0.49, 0.49) so we dont need to store any
   fixed point integer bits. *)

let scale_coefs ~precision coefs =
  let scale_factor = Float.(2. ** of_int precision) in
  Array.map coefs ~f:(fun c ->
      let scaled = Float.(c * scale_factor) in
      Float.round_nearest_half_to_even scaled
      |> Float.to_int
      |> of_int ~width:(precision + 1))
;;

let _mul4 ~precision coefs x =
  let coefs = scale_coefs ~precision coefs |> Array.to_list in
  let x = List.map coefs ~f:(( *+ ) x) in
  tree ~arity:2 ~f:(reduce ~f:Sop.( +: )) x
;;

let _idct_4pt _b = ()

(* Multiplier based implementations *)

module Single_multiplier = struct
  let input_bits = 12
  let transpose_bits = 12

  module I = struct
    type 'a t =
      { clocking : 'a Clocking.t
      ; input_coef : 'a [@bits input_bits]
      ; transpose_coef : 'a [@bits transpose_bits]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { write_enable : 'a [@bits 2]
      ; write_address : 'a [@bits 6]
      ; read_address : 'a [@bits 6]
      ; write_coef : 'a [@bits 8]
      ; read_enable : 'a [@bits 2]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module State = struct
    type t =
      | Start
      | Loop
      | Last
    [@@deriving sexp_of, compare, enumerate]
  end

  let _for_ clocking lo hi f =
    let range = lo - hi + 1 in
    let log_range = Int.ceil_log2 range in
    let index =
      Clocking.Var.reg clocking ~clear_to:(of_int ~width:log_range 0) ~width:log_range
    in
    Always.(
      proc
        [ index <-- index.value +:. 1
        ; f index.value
        ; when_ (index.value ==:. hi) [ index <--. lo ]
        ])
  ;;

  module Loop = struct
    type t =
      { lo : int
      ; hi : int
      ; on_each_update : Signal.t -> Always.t
      ; on_last_update : unit -> Always.t
      }

    let init
        ?(on_each_update = fun _ -> Always.proc [])
        ?(on_last_update = fun () -> Always.proc [])
        lo
        hi
      =
      { lo; hi; on_each_update; on_last_update }
    ;;

    let create ~clocking t =
      let log_range = Int.ceil_log2 (t.hi - t.lo + 1) in
      let index =
        Clocking.Var.reg
          clocking
          ~clear_to:(of_int ~width:log_range t.lo)
          ~width:log_range
      in
      let stat =
        Always.(
          proc
            [ index <-- index.value +:. 1
            ; t.on_each_update index.value
            ; when_ (index.value ==:. t.hi) [ t.on_last_update (); index <--. t.lo ]
            ])
      in
      stat, index.value
    ;;
  end

  module Nested_loops = struct
    type t = Loop.t array

    let create ~clocking (t : t) =
      Array.fold_right
        t
        ~init:(Always.proc [], [])
        ~f:(fun loop (stat, vars) ->
          let stat, index =
            Loop.create
              ~clocking
              { loop with
                on_last_update = (fun () -> Always.proc [ loop.on_last_update (); stat ])
              }
          in
          stat, index :: vars)
    ;;
  end

  let create (i : _ I.t) =
    let sm = Clocking.state_machine (module State) i.clocking in
    let loop, indexes =
      Nested_loops.create ~clocking:i.clocking [| Loop.init 0 3; Loop.init 0 15 |]
    in
    Always.(
      compile
        [ sm.switch
            [ Start, [ sm.set_next Loop ]; Loop, [ loop ]; Last, [ sm.set_next Start ] ]
        ]);
    { O.write_enable = zero 2
    ; write_address = concat_lsb indexes
    ; read_address = zero 6
    ; write_coef = zero 8
    ; read_enable = zero 2
    }
  ;;
end

module Simple = struct
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
    let mul = Clocking.reg i.clocking (i.coef *+ idct_rom ~row:y.value ~col:z.value) in
    let mac =
      let mac_bits = input_bits + rom_bits + 3 in
      reg_fb (Clocking.to_spec i.clocking) ~enable:vdd ~width:mac_bits ~f:(fun d ->
          d +: sresize mul mac_bits)
    in
    let clipped_and_rounded_pixel =
      let needs_rounding = mac.:(rom_bits - 1) in
      let is_negative = msb mac in
      let mac = mac.:[width mac - 1, rom_bits] in
      let mac =
        (* rounds away from 0 *)
        mux2 needs_rounding (mux2 is_negative Sop.(mac -: vdd) Sop.(mac +: vdd)) (se mac)
      in
      let pixel_max = 1 lsl pixel_bits in
      mux2
        (mac <+. -(pixel_max / 2))
        (zero pixel_bits)
        (mux2 (mac >+. (pixel_max / 2) - 1) (ones pixel_bits) mac.:[7, 0])
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
end
