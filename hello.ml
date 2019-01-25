open Base

let play_note ?(velo = 1.0) midi len =
  let open Signal in
  let open Shape in
  saw (Temperament.equal midi)
  (*|> add (saw (1.011742 *. Temperament.equal midi))
  |> add (saw (0.99837 *. Temperament.equal midi))*)
  |> mul (decay (Float.max (len /. 3.0) 0.2))
  |> gain (0.05 *. velo)
  |> crop len
;;

let song_demo () =
  let song =
    let scale = Scale.major 69 in
    [0; 1; 2; 3; 4; 5; 6; 7; 7; 6; 5; 4; 3; 2; 1; 0; 0; 3; 5; 7; 5; 3; 0]
    |> List.mapi ~f:(fun i note ->
      (Float.of_int i) *. 0.2, play_note (Scale.at scale note) 1.0)
    |> Signal.render
  in
  Wav.save ~sampling_rate:44100 "song.wav" [song];
;;

let midi_demo () =
  let midi_file = Midi.read_file "data/spider_dance.mid" in
  let time_scale = 1. in
  let offsets_and_signals =
    List.map (List.concat midi_file.tracks) ~f:(fun { midi; time; dur; velo; _ } ->
      (time *. time_scale), play_note midi (dur *. time_scale) ~velo:(Float.of_int velo /. 255.))
  in
  Wav.save ~sampling_rate:44100 "output.wav" [Signal.render offsets_and_signals];
;;

let frame_demo () =
  let midi_file = Midi.read_file "data/necrofantasia.mid" in
  Map.iteri midi_file.key_signatures ~f:(fun ~key ~data ->
    Stdlib.Printf.printf "At %f: Key signature %d\n" key data.accidentals);
  let track = List.concat midi_file.tracks in
  let min_note, max_note =
    List.fold track
      ~init:(Int.max_value, Int.min_value)
      ~f:(fun (min, max) { midi; _ } ->
        Int.min min midi, Int.max max midi)
  in
  let max_time =
    List.fold track ~init:0. ~f:(fun max { time; _ } -> Float.max max time)
  in
  Stdlib.Printf.printf "Note range: %d-%d\n" min_note max_note;
  Stdlib.Printf.printf "Duration: %f\n" max_time;
  let frame = Draw.make_frame () in
  Draw.(fill frame (rgb 0 128 192));
  let midi_to_x = Draw.piano frame 24 95 in
  for midi = 24 to 96 do
    let x = midi_to_x midi in
    Draw.(rect frame (x - 1) 0 2 (1080 - 150) (rgb 64 64 64))
  done;
  List.iter track ~f:(fun { midi; time; dur; _ } ->
    let x = midi_to_x midi in
    let x' = midi_to_x (midi + 1) in
    let start_y = 1080 - 150 - Int.of_float (time *. 120.) in
    let end_y = 1080 - 150 - Int.of_float ((time +. dur) *. 120.) in
    Draw.(rect frame (x + 1) end_y (x' - x - 2) (start_y - end_y) (rgb 255 255 255))
  );
  frame#save "cake.png" None [];
;;

let () =
  frame_demo ();
  midi_demo ()
;;
