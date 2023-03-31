open Core

let marker_length bits ~pos =
  List.init 2 ~f:(fun i -> Char.to_int bits.[pos + i])
  |> List.fold ~init:0 ~f:(fun acc a -> (acc lsl 8) lor a)
;;

let find_next_marker ~start_pos ~marker_code bits =
  let marker = Char.of_int_exn marker_code in
  With_return.with_return_option (fun r ->
      for pos = start_pos to String.length bits - 2 do
        if Char.equal bits.[pos] '\xff' && Char.equal bits.[pos + 1] marker
        then r.return pos
      done)
;;

let find_next_marker_exn ~start_pos ~marker_code bits =
  find_next_marker ~start_pos ~marker_code bits |> Option.value_exn
;;

let extract_next_marker ~start_pos ~marker_code bits =
  let pos = find_next_marker ~start_pos ~marker_code bits in
  Option.map pos ~f:(fun pos ->
      let pos = pos + 2 in
      let len = marker_length bits ~pos in
      String.sub ~pos ~len bits)
;;

let extract_next_marker_exn ~start_pos ~marker_code bits =
  extract_next_marker ~start_pos ~marker_code bits |> Option.value_exn
;;
