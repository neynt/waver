open! Base

let button =
  let open Signal in
  noise ()
  |> crop 0.2
  |> mul (decay 0.02)
  |> gain 0.2
;;

let screen_transition =
  let open Signal in
  brown_noise ()
  |> gain 0.1
  |> crop 1.0
  |> mul (pwlin [0.3, 1.0; 0.4, 1.0; 0.3, 0.0])
  |> mul (decay 0.3)
  |> gain 0.2
;;

let lose_health =
  let open Signal in
  square 370.
  |> gain 0.08
  |> chirp_exp 1.0 0.5 0.1
  |> mul (decay 0.2)
  |> crop 0.3
;;

let fire_spell =
  let open Signal in
  sine 500.
  |> gain 0.2
  |> phase_mod (ramp |> add (sine 7. |> gain 0.0005 |> mul (pwlin [0.2, 1.0; 100., 1.0])))
  |> mul (decay 0.2)
  |> mul (pwlin [0.2, 1.0; 0.5, 0.0])
  |> crop 0.7
;;

let trigger filename =
  Wav.save ~sampling_rate:44100 filename [screen_transition]
;;

let render prefix =
  let save signal filename =
    Caml.Printf.printf "%s\n" filename;
    Wav.save ~sampling_rate:44100 (Caml.Filename.concat prefix filename) [signal]
  in
  save button "button.wav";
  save screen_transition "screen_transition.wav";
  save lose_health "lose_health.wav";
  save fire_spell "fire_spell.wav";
;;
