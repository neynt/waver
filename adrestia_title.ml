open Base

module Comp = Composition.Make ()

module Percussion = struct
  type note =
    | Hits of char list
    | Rest of int
  [@@deriving sexp]

  type t = note list
  [@@deriving sexp]

  let of_text_notation =
    let open Angstrom in
    let spaces = skip_while (function | ' ' | '\n' -> true | _ -> false) in
    let lex p = p <* spaces in
    let group = lift (fun x -> Hits (String.to_list x)) (take_while1 Char.is_alpha) in
    let rest = lift (fun x -> Rest (String.count x ~f:(Char.(=) '.'))) (take_while1 (function | '.' -> true | _ -> false)) in
    let beat = group <|> rest in
    let percussion = spaces *> many (lex beat) in
    fun s ->
      parse_string percussion s
      |> Result.ok_or_failwith

  let render t (sounds: (char * Signal.t) List.t) =
    let sounds = Map.of_alist_exn (module Char) sounds in
    let _time, signals =
      List.fold t ~init:(0, []) ~f:(fun (time, signals) note ->
        match note with
        | Hits chars -> time + 1, List.map chars ~f:(Map.find_exn sounds)
        | Rest dur -> time + dur, signals)
    in
    signals
end

let song_demo output_file =
  let open Comp in
  let open Instrument in
  bpm := 240.;

  let melody scale text_notation =
    let notes = Melody.of_text_notation text_notation in
    List.iter notes ~f:(fun { content; length } ->
      match content with
      | `Pitch { index; accidental } ->
        play length (blip ((Scale.at scale index) + accidental) (bt length))
      | `Rest ->
        advance length);
  in

  let play_perc text_notation (sounds: (char * Signal.t) List.t) =
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
  in

  let scale = Scale.major 64 in
  let bass_scale = Scale.shift_octaves scale (-2) in

  together [
    (fun () -> play_perc {|
      . s . s . s . s
      . s . s . s . s
      . s . s . s . s
      . s . s . s . s
    |} ['k', kick; 's', hihat]);
    (fun () -> melody bass_scale {|
      3 7 5 7 3 7 5 7
      4 6 8 6 4 6 8 6
      2 6 4 6 2 6 4 6
      5 7 9 7 5 7 9 7
    |});
  ];

  together [
    (fun () -> play_perc {|
      k s . s k s . s
      k s . s k s . s
      k s . s k s . s
      k s . s k s . s
    |} ['k', kick; 's', hihat]);
    (fun () -> melody bass_scale {|
      3 7 5 7 3 7 5 7
      4 6 8 6 4 6 8 6
      2 6 4 6 2 6 4 6
      5 7 9 7 5 7 9 7
    |});
    (fun () -> melody scale {|
      0 - 3 - 4 - 5 -
      6 - - - 7 6 5 6
      9 - - - 8 7 6 5
      2 - 3 - 4 - - -
    |});
  ];

  together [
    (fun () -> play_perc {|
      k h . h k h . h
      k h . h k h . h
      k h . h k h . h
      k h . h k h . h
    |} ['k', kick; 'h', hihat]);
    (fun () -> melody bass_scale {|
      0 4 2 4 0 4 2 4
      5 9 7 9 5 9 7 9
      3 5 7 5 3 5 7 5
      4 6 8 6 4 6 8 6
    |});
    (fun () -> melody scale {|
      0 - - - 1 - - -
      2 - - - 3 - - -
      5 - - - 6 - - -
      4 - - - - - - -
    |});
  ];

  let song = Signal.render (result ()) in
  Wav.save ~sampling_rate:44100 output_file [song];
;;
