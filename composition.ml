open Base

module Percussion = struct
  type note = Hits of char list | Rest of int [@@deriving sexp]

  type t = note list [@@deriving sexp]

  let of_text_notation =
    let open Angstrom in
    let spaces = skip_while (function ' ' | '\n' -> true | _ -> false) in
    let lex p = p <* spaces in
    let group =
      lift (fun x -> Hits (String.to_list x)) (take_while1 Char.is_alpha)
    in
    let rest =
      lift
        (fun x -> Rest (String.count x ~f:(Char.( = ) '.')))
        (take_while1 (function '.' -> true | _ -> false))
    in
    let beat = group <|> rest in
    let percussion = spaces *> many (lex beat) in
    fun s -> parse_string percussion s |> Result.ok_or_failwith

  let render t (sounds : (char * Signal.t) List.t) =
    let sounds = Map.of_alist_exn (module Char) sounds in
    let _time, signals =
      List.fold t ~init:(0, []) ~f:(fun (time, signals) note ->
          match note with
          | Hits chars -> (time + 1, List.map chars ~f:(Map.find_exn sounds))
          | Rest dur -> (time + dur, signals))
    in
    signals
end

(* Environment for composing pieces. Currently relies a lot on mutability for a
 * nicer interface. *)
module Make () = struct
  let notes : (float * Signal.t) list ref = ref []

  let bpm : float ref = ref 120.

  let pos : float ref = ref 0.

  let pos_start : float ref = ref 0.

  let pos_end : float ref = ref Float.infinity

  let marks : float list ref = ref []

  let bt' i = i *. (60. /. !bpm)

  let bt i = bt' (Float.of_int i)

  let at' beat signal = notes := (bt' (!pos +. beat), signal) :: !notes

  let at beat = at' (Float.of_int beat)

  let advance' beats = pos := !pos +. beats

  let advance beats = advance' (Float.of_int beats)

  let seek' beats = pos := beats

  let seek beats = pos := Float.of_int beats

  let play beats signal =
    at 0 signal;
    advance beats

  let start_here () = pos_start := !pos

  let end_here () = pos_end := !pos

  (* Marks allow you to save positions in the track and return to them. *)
  let push_mark () = marks := !pos :: !marks

  let set_mark () =
    marks := match !marks with [] -> [ !pos ] | _ :: tl -> !pos :: tl

  let pop_mark () =
    match !marks with
    | hd :: tl ->
        pos := hd;
        marks := tl
    | _ -> failwith "empty mark stack"

  let get_mark () = pos := List.hd_exn !marks

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

  let result () =
    List.filter !notes ~f:(fun (t, _) -> Float.(t < bt' !pos_end))
    |> List.map ~f:(fun (t, s) -> (t -. bt' !pos_start, s))
    |> List.rev

  let melody ~instrument scale text_notation =
    let notes = Melody.of_text_notation text_notation in
    List.iter notes ~f:(fun { content; length } ->
        match content with
        | `Pitch { index; accidental } ->
            play length
              (instrument (Scale.at scale index + accidental) (bt length))
        | `Rest -> advance length)

  let play_perc text_notation (sounds : (char * Signal.t) List.t) =
    let percussion = Percussion.of_text_notation text_notation in
    let sounds = Map.of_alist_exn (module Char) sounds in
    let _time =
      List.iter percussion ~f:(fun note ->
          match note with
          | Hits chars ->
              List.iter chars ~f:(fun c -> at 0 (Map.find_exn sounds c));
              advance 1
          | Rest dur -> advance dur)
    in
    ()
end
