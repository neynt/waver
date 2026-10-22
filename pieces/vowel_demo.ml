(** Vowel formant A/B — plays each vowel twice (A4 then E5) so the formant
    character is audible independent of pitch. Order: ah, ee, oo, oh, eh, uh. *)
open Core

open Waver
module Compo = Composition.Make ()

let render output_file =
  let open Compo in
  let module C = Chiptune in
  bpm := 60.;
  let note_dur = 1.0 in
  let gap = 0.25 in
  let long_gap = 0.6 in
  let vowels =
    [ "ah", C.ah
    ; "ee", C.ee
    ; "oo", C.oo
    ; "oh", C.oh
    ; "eh", C.eh
    ; "uh", C.uh
    ]
  in
  (* 0.5s lead-in silence. *)
  advance' 0.5;
  List.iter vowels ~f:(fun (_, synth) ->
    at' 0. (synth ~velo:0.9 69 note_dur);
    advance' (note_dur +. gap);
    at' 0. (synth ~velo:0.9 76 note_dur);
    advance' (note_dur +. long_gap));
  let song = Signal.render (result ()) |> Signal.gain 0.85 in
  Wav.save ~sampling_rate:44100 output_file [ song ]
