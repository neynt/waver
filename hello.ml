open Base

module Scale = struct
  type t =
    { tonic: int
    ; period: int
    ; offsets: int array
    }

  type degree =
    { index: int
    ; accidental: [`Sharp | `Flat | `Natural]
    }

  let major tonic =
    { tonic
    ; period = 12
    ; offsets = [|0; 2; 4; 5; 7; 9; 11|]
    }

  let natural_minor tonic =
    { tonic
    ; period = 12
    ; offsets = [|0; 2; 3; 5; 7; 8; 10|]
    }

  let at { tonic; period; offsets } i =
    let divmod a b = a / b, a % b in
    let octave, ofs = divmod i (Array.length offsets) in
    tonic + (period * octave) + Array.get offsets ofs

  (*
  let range { tonic; period; offsets } lower upper =
    failwith "unimplemented"
  ;;
  *)
end

module Temperament = struct
  type t = int -> float
  let equal midi = 440. *. 2. **. ((Float.of_int (midi - 69)) /. 12.)
end

let play_note ?(velo = 1.0) midi len =
  let open Signal in
  let open Shape in
  saw (Temperament.equal midi)
  |> mul (decay (Float.max (len /. 3.0) 0.2))
  |> gain (0.05 *. velo)
  |> crop len

let song =
  let scale = Scale.major 69 in
  [0; 1; 2; 3; 4; 5; 6; 7; 7; 6; 5; 4; 3; 2; 1; 0; 0; 3; 5; 7; 5; 3; 0]
  |> List.mapi ~f:(fun i note ->
    (Float.of_int i) *. 0.2, play_note (Scale.at scale note) 1.0)
  |> Signal.render
;;

let () =
  let midi_file = Midi.read_file "data/spider_dance.mid" in
  let offsets_and_signals =
    List.map (List.concat midi_file.tracks) ~f:(fun { midi; time; dur; velo; _ } ->
      time, play_note midi dur ~velo:(Float.of_int velo /. 255.))
  in
  Wav.save ~sampling_rate:44100 "output.wav" [Signal.render offsets_and_signals];
  Wav.save ~sampling_rate:44100 "song.wav" [song];
  ()
;;
