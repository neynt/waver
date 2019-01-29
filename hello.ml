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
  let deg_to_color deg =
    let open Draw in
    match deg with
    | 0 -> rgb 255 0 0
    | 1 -> rgb 255 128 0
    | 2 -> rgb 255 255 0
    | 3 -> rgb 0 255 0
    | 4 -> rgb 0 255 255
    | 5 -> rgb 0 128 255
    | 6 -> rgb 128 0 255
    | _ -> failwith "Scale degree out of range"
  in
  let midi_file = Midi.read_file "data/necrofantasia.mid" in
  let track = List.concat midi_file.tracks in
  let min_note, max_note =
    List.fold track
      ~init:(Int.max_value, Int.min_value)
      ~f:(fun (min, max) { midi; _ } ->
        Int.min min midi, Int.max max midi)
  in
  let min_note = Int.round_down ~to_multiple_of:12 (min_note - 1) in
  let max_note = Int.round_up ~to_multiple_of:12 (max_note + 1) - 1 in
  let max_time =
    List.fold track ~init:0. ~f:(fun max { time; _ } -> Float.max max time)
  in
  Map.iteri midi_file.key_signatures ~f:(fun ~key ~data ->
    Stdlib.Printf.printf "At %f: Time signature %d\n" key data.accidentals);
  let save_frame time_ofs filename =
    let frame = Draw.make_frame () in
    Draw.(fill frame (rgb 16 16 16));
    let midi_to_x = Draw.piano ~semitone_width:15 frame min_note max_note in
    for midi = min_note to max_note + 1 do
      let x = midi_to_x midi in
      Draw.(rect frame (x - 1) 0 2 (1080 - 150) (rgb 32 32 32))
    done;
    List.iter track ~f:(fun { midi; time; dur; _ } ->
      let time = time -. time_ofs in
      let { Midi. quality; accidentals } = Midi.key_signature_at midi_file time in
      let x = midi_to_x midi in
      let x' = midi_to_x (midi + 1) in
      let start_y = Int.min (1080 - 150) (1080 - 150 - Int.of_float (time *. 120.)) in
      let end_y = 1080 - 150 - Int.of_float ((time +. dur) *. 120.) in
      let h = start_y - end_y in
      let scale = Scale.from_accidental_count ~quality accidentals in
      let deg = Scale.scale_degree scale midi in
      let color = deg_to_color deg.index in
      match deg.accidental with
      | `Natural ->
        Draw.(rect frame (x + 1) end_y (x' - x - 2) h color)
      | `Flat ->
        let color2 = deg_to_color ((deg.index - 1) % Array.length scale.offsets) in
        Draw.(striped_rect frame (x + 1) end_y (x' - x - 2) h 5 color color2)
      | `Sharp ->
        let color2 = deg_to_color ((deg.index + 1) % Array.length scale.offsets) in
        Draw.(striped_rect frame (x + 1) end_y (x' - x - 2) h 5 color color2)
    );
    frame#save filename None [];
  in
  Stdlib.Printf.printf "Duration: %f\n" max_time;
  let rec _loop time_ofs =
    match Float.(time_ofs > max_time) with
    | true -> ()
    | false ->
      save_frame time_ofs (Stdlib.Printf.sprintf "output/cake.%08.3f.png" time_ofs);
      _loop (time_ofs +. 10.)
  in
  save_frame 22. (Stdlib.Printf.sprintf "cake.png");
;;

let () =
  frame_demo ();
  midi_demo ()
;;
