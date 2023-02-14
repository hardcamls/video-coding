(* Test fixed point DCT against reference DCT and show errors. *)

open Core
module Dct = Hardcaml_jpeg_model.Dct

let inputs ~range =
  Array.init 8 ~f:(fun _ -> Array.init 8 ~f:(fun _ -> Random.int (range * 2) - range))
;;

let transform
    ~verbose
    ~rom_prec
    ~transpose_prec
    ~fixed_transform
    ~reference_transform
    inputs
  =
  let dct = fixed_transform ~rom_prec ~transpose_prec inputs in
  let ref_dct = reference_transform (Dct.Matrix8x8.map inputs ~f:Float.of_int) in
  let error = Dct.Matrix8x8.map2 dct ref_dct ~f:(fun a b -> Float.(abs (of_int a - b))) in
  let max_error =
    Array.fold error ~init:0. ~f:(fun m a ->
        Array.fold a ~init:m ~f:(fun m a -> if Float.(abs a > m) then Float.abs a else m))
  in
  if verbose
  then
    print_s
      [%message
        (inputs : int array array)
          (dct : int array array)
          (ref_dct : float array array)
          (max_error : float)
          (error : float array array)];
  max_error
;;

module Args = struct
  type t =
    { rom_prec : int
    ; transpose_prec : int
    ; range : int
    ; count : int
    }

  open Command.Param

  let rom_prec prefix =
    flag [%string "%{prefix}-rom-prec"] (optional_with_default 12 int) ~doc:""
  ;;

  let transpose_prec prefix =
    flag [%string "%{prefix}-transpose-prec"] (optional_with_default 2 int) ~doc:""
  ;;

  let range = flag "-input-range" (optional_with_default 200 int) ~doc:""
  let count = flag "-count" (optional_with_default 1 int) ~doc:""

  let arg =
    [%map_open.Command
      let rom_prec = rom_prec ""
      and transpose_prec = transpose_prec ""
      and range = range
      and count = count in
      { rom_prec; transpose_prec; range; count }]
  ;;
end

let command_forward =
  Command.basic
    ~summary:"Perform a forward dct using fixed point and compare to reference"
    [%map_open.Command
      let { rom_prec; transpose_prec; range; count } = Args.arg in
      fun () ->
        let max_error = ref 0. in
        for _ = 1 to count do
          let inputs = inputs ~range in
          let error =
            transform
              ~verbose:(count = 1)
              ~rom_prec
              ~transpose_prec
              ~fixed_transform:Dct.Fixed_point.forward_transform
              ~reference_transform:Dct.Floating_point.Eight_point.forward_transform
              inputs
          in
          max_error := if Float.(error > !max_error) then error else !max_error
        done;
        if count > 1 then print_s [%message (max_error : float ref)]]
;;

let command_inverse =
  Command.basic
    ~summary:"Perform an inverse dct using fixed point and compare to reference"
    [%map_open.Command
      let { rom_prec; transpose_prec; range; count } = Args.arg in
      fun () ->
        let max_error = ref 0. in
        for _ = 1 to count do
          let inputs = inputs ~range in
          let error =
            transform
              ~verbose:(count = 1)
              ~rom_prec
              ~transpose_prec
              ~fixed_transform:Dct.Fixed_point.inverse_transform
              ~reference_transform:Dct.Floating_point.Eight_point.inverse_transform
              inputs
          in
          max_error := if Float.(error > !max_error) then error else !max_error
        done;
        if count > 1 then print_s [%message (max_error : float ref)]]
;;

let forward_and_inverse
    ~verbose
    ~fwd_rom_prec
    ~fwd_transpose_prec
    ~inv_rom_prec
    ~inv_transpose_prec
    inputs
  =
  let ref_dct =
    Dct.Floating_point.Eight_point.forward_transform
      (Dct.Matrix8x8.map inputs ~f:Float.of_int)
    |> Dct.Floating_point.Eight_point.inverse_transform
  in
  let fixed_dct =
    Dct.Fixed_point.forward_transform
      ~rom_prec:fwd_rom_prec
      ~transpose_prec:fwd_transpose_prec
      inputs
    |> Dct.Fixed_point.inverse_transform
         ~rom_prec:inv_rom_prec
         ~transpose_prec:inv_transpose_prec
  in
  let ref_error =
    Dct.Matrix8x8.map2 inputs ref_dct ~f:(fun a b -> Float.(abs (of_int a - b)))
  in
  let max_ref_error =
    Array.fold ref_error ~init:0. ~f:(fun m a ->
        Array.fold a ~init:m ~f:(fun m a -> if Float.(abs a > m) then Float.abs a else m))
  in
  let fixed_error =
    Dct.Matrix8x8.map2 inputs fixed_dct ~f:(fun a b -> Int.(abs (a - b)))
  in
  let max_fixed_error =
    Array.fold fixed_error ~init:0 ~f:(fun m a ->
        Array.fold a ~init:m ~f:(fun m a -> if Int.(abs a > m) then Int.abs a else m))
  in
  if verbose
  then
    print_s
      [%message
        (inputs : int array array)
          (fixed_dct : int array array)
          (ref_dct : float array array)
          (max_ref_error : float)
          (max_fixed_error : int)];
  max_fixed_error
;;

let command_both =
  Command.basic
    ~summary:"Perform and forward then inverse transform and check the overall error"
    [%map_open.Command
      let fwd_rom_prec = Args.rom_prec "-fwd"
      and fwd_transpose_prec = Args.transpose_prec "-fwd"
      and inv_rom_prec = Args.rom_prec "-inv"
      and inv_transpose_prec = Args.transpose_prec "-inv"
      and count = Args.count in
      fun () ->
        let max_error = ref 0 in
        for _ = 1 to count do
          let inputs = inputs ~range:128 in
          let error =
            forward_and_inverse
              ~verbose:(count = 1)
              ~fwd_rom_prec
              ~fwd_transpose_prec
              ~inv_rom_prec
              ~inv_transpose_prec
              inputs
          in
          max_error := if error > !max_error then error else !max_error
        done;
        if count > 1 then print_s [%message (max_error : int ref)]]
;;

let command_search =
  Command.basic
    ~summary:"Search over parameter range and display error"
    [%map_open.Command
      let count = flag "-count" (optional_with_default 10_000 int) ~doc:"" in
      fun () ->
        for fwd_rom_prec = 8 to 16 do
          for fwd_transpose_prec = 0 to 5 do
            for inv_rom_prec = 8 to 16 do
              for inv_transpose_prec = 0 to 5 do
                let max_error = ref 0 in
                for _ = 1 to count do
                  let inputs = inputs ~range:128 in
                  let error =
                    forward_and_inverse
                      ~verbose:false
                      ~fwd_rom_prec
                      ~fwd_transpose_prec
                      ~inv_rom_prec
                      ~inv_transpose_prec
                      inputs
                  in
                  max_error := if error > !max_error then error else !max_error
                done;
                printf
                  "%2i %2i %2i %2i - %i\n%!"
                  fwd_rom_prec
                  fwd_transpose_prec
                  inv_rom_prec
                  inv_transpose_prec
                  !max_error
              done
            done
          done
        done]
;;

let command_idct_hardware =
  Command.basic
    ~summary:""
    [%map_open.Command
      let seed = flag "-seed" (optional int) ~doc:"" in
      fun () ->
        Option.iter seed ~f:Random.init;
        let inputs = Hardcaml_jpeg_test.Test_dct.create_inputs () in
        Hardcaml_jpeg_test.Test_dct.reference inputs;
        let waves = Hardcaml_jpeg_test.Test_dct.(simulate_idct inputs) in
        Hardcaml_waveterm_interactive.run waves]
;;

let () =
  Command_unix.run
    (Command.group
       ~summary:""
       [ "forward", command_forward
       ; "inverse", command_inverse
       ; "both", command_both
       ; "search", command_search
       ; "hardware", command_idct_hardware
       ])
;;
