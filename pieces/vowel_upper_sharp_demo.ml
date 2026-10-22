(** Upper-sharp A/B — each vowel with F1 left wide (sharpness 1.0) and F2-F4
    narrowed: sharpness_upper = 1.0, 1.5, 2.5, 3.5. Body stays intact (F1 carries
    F0) while upper formants get brighter character. *)
open Core

open Waver
module Compo = Composition.Make ()

let render output_file =
  let open Compo in
  let module C = Chiptune in
  bpm := 60.;
  let note_dur = 1.1 in
  let gap = 0.25 in
  let long_gap = 0.8 in
  let midi = 69 in
  let vowels =
    [ ( "ah"
      , fun ?sharpness_upper v m d -> C.ah ?sharpness_upper ~velo:v m d )
    ; ( "ee"
      , fun ?sharpness_upper v m d -> C.ee ?sharpness_upper ~velo:v m d )
    ; ( "oo"
      , fun ?sharpness_upper v m d -> C.oo ?sharpness_upper ~velo:v m d )
    ]
  in
  let variants = [ 1.0; 1.5; 2.5; 3.5 ] in
  advance' 0.4;
  List.iter vowels ~f:(fun (_, synth) ->
    List.iter variants ~f:(fun shu ->
      at' 0. (synth ~sharpness_upper:shu 0.9 midi note_dur);
      advance' (note_dur +. gap));
    advance' long_gap);
  let song = Signal.render (result ()) |> Signal.gain 0.85 in
  Wav.save ~sampling_rate:44100 output_file [ song ]
