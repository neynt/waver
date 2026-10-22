(** Floor A/B — each vowel at sharpness 2.5 with floor = 0, 0.05, 0.15, 0.30.
    Floor sets a minimum formant_gain so harmonics between peaks still pass,
    keeping the fundamental audible even with sharp formants. *)
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
      , fun ?sharpness ?floor v m d -> C.ah ?sharpness ?floor ~velo:v m d )
    ; ( "ee"
      , fun ?sharpness ?floor v m d -> C.ee ?sharpness ?floor ~velo:v m d )
    ; ( "oo"
      , fun ?sharpness ?floor v m d -> C.oo ?sharpness ?floor ~velo:v m d )
    ]
  in
  (* (sharpness, floor) — first is baseline for reference. *)
  let variants = [ 1.0, 0.; 2.5, 0.; 2.5, 0.05; 2.5, 0.15; 2.5, 0.30 ] in
  advance' 0.4;
  List.iter vowels ~f:(fun (_, synth) ->
    List.iter variants ~f:(fun (sh, fl) ->
      at' 0. (synth ~sharpness:sh ~floor:fl 0.9 midi note_dur);
      advance' (note_dur +. gap));
    advance' long_gap);
  let song = Signal.render (result ()) |> Signal.gain 0.85 in
  Wav.save ~sampling_rate:44100 output_file [ song ]
