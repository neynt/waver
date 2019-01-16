open Base

let pi = 4.0 *. Float.atan 1.0
;;

let sine_wave freq t =
  Float.sin (t *. 2. *. pi *. freq)
;;

module Temperament = struct
  type t = int -> float
  let equal midi = 440. *. 2. **. ((Float.of_int (midi - 69)) /. 12.)
end;;

let memo f m =
  let table = Hashtbl.create m in
  (fun x -> Hashtbl.find_or_add table x ~default:(fun () -> f x))
;;

let play_note midi =
  let open Signal in
  create (sine_wave (Temperament.equal midi)) 0.5
  |> gain 0.03
  |> crop 0.2
;;

let play_note = memo play_note (module Int)
;;

let song =
  Signal.render
  [ 0.0, play_note 69
  ; 0.2, play_note 73
  ; 0.4, play_note 76
  ; 0.6, play_note 81
  ]
;;

let () =
  Wav.save ~sampling_rate:44100 "output.wav" [song];
  Stdlib.print_endline "wat";
  let midi_file = Midi.do_stuff () in
  Caml.Printf.printf "Midi has time unit %f\n" midi_file.time_unit;
  let offsets_and_signals =
    List.map (List.concat midi_file.tracks) ~f:(fun { midi; time; _ } ->
      Float.of_int time /. midi_file.time_unit /. 4., play_note midi)
  in
  Wav.save ~sampling_rate:44100 "mogeko.wav" [Signal.render offsets_and_signals];
  ()
;;
