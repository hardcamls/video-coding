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
