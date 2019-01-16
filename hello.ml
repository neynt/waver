open Base

let pi = 4.0 *. Float.atan 1.0
;;

let sine_wave freq t =
  Float.sin (t *. 2. *. pi *. freq)
;;

let saw_wave freq t =
  (Float.mod_float (t *. freq) 1.) *. 2. -. 1.
;;

module Temperament = struct
  type t = int -> float
  let equal midi = 440. *. 2. **. ((Float.of_int (midi - 69)) /. 12.)
end;;

let memo f m =
  let table = Hashtbl.create m in
  (fun x -> Hashtbl.find_or_add table x ~default:(fun () -> f x))
;;

let play_note ?(velo = 1.0) midi len =
  Signal.add
    (Signal.create (saw_wave (0.997 *. Temperament.equal midi)) len
     |> Signal.gain (0.05 *. velo))
    (Signal.create (saw_wave (1.01 *. Temperament.equal midi)) len
     |> Signal.gain (0.05 *. velo))
;;

let song =
  Signal.render
  [ 0.0, play_note 69 0.2
  ; 0.2, play_note 73 0.2
  ; 0.4, play_note 76 0.2
  ; 0.6, play_note 81 0.2
  ]
;;

let () =
  let midi_file = Midi.read_file "data/spider_dance.mid" in
  let offsets_and_signals =
    List.map (List.concat midi_file.tracks) ~f:(fun { midi; time; dur; velo; _ } ->
      time, play_note midi dur ~velo:(Float.of_int velo /. 255.))
  in
  Wav.save ~sampling_rate:44100 "output.wav" [Signal.render offsets_and_signals];
  ()
;;
