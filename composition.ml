open Base

(* Environment for composing pieces. Currently relies a lot on mutability for a
 * nicer interface. *)
module Make () = struct
  let notes: (float * Signal.t) list ref = ref []
  let bpm: float ref = ref 120.
  let pos: float ref = ref 0.
  let pos_start: float ref = ref 0.
  let pos_end: float ref = ref Float.infinity
  let marks: float list ref = ref []

  let bt' i = i *. (60. /. !bpm)
  let bt i = bt' (Float.of_int i)
  let at' beat signal = notes := (bt' (!pos +. beat), signal) :: !notes
  let at beat = at' (Float.of_int beat)
  let advance' beats = pos := !pos +. beats
  let advance beats = advance' (Float.of_int beats)
  let seek' beats = pos := beats
  let seek beats = pos := (Float.of_int beats)
  let play beats signal =
    at 0 signal;
    advance beats;
  ;;

  let start_here () = pos_start := !pos
  let end_here () = pos_end := !pos
  let push_mark () = marks := !pos :: !marks
  let set_mark () =
    marks := match !marks with
      | [] -> [!pos]
      | _ :: tl -> !pos :: tl
  ;;
  let pop_mark () =
    match !marks with
    | hd :: tl ->
      pos := hd;
      marks := tl
    | _ -> failwith "empty mark stack"
  ;;
  let get_mark () =
    pos := List.hd_exn !marks
  ;;

  let together parts =
    let latest_pos =
      List.fold ~init:!pos parts ~f:(fun latest_pos part ->
        set_mark ();
        part ();
        let result = Float.max latest_pos !pos in
        get_mark ();
        result)
    in
    pos := latest_pos
  ;;

  let result () =
    List.filter !notes ~f:(fun (t, _) -> Float.(t < bt' !pos_end))
    |> List.map ~f:(fun (t, s) -> (t -. bt' !pos_start, s))
    |> List.rev
  ;;
end
