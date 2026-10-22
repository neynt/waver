(** Exaggeration A/B — plays ah/ee/oo at sharpness 1.0, 2.0, 3.0 on A4 so the
    effect of narrowing the formant bandwidths is audible back-to-back. *)
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
    [ "ah", (fun ?sharpness v m d -> C.ah ?sharpness ~velo:v m d)
    ; "ee", (fun ?sharpness v m d -> C.ee ?sharpness ~velo:v m d)
    ; "oo", (fun ?sharpness v m d -> C.oo ?sharpness ~velo:v m d)
    ]
  in
  advance' 0.4;
  List.iter vowels ~f:(fun (_, synth) ->
    List.iter [ 1.0; 2.0; 3.0 ] ~f:(fun sh ->
      at' 0. (synth ~sharpness:sh 0.9 midi note_dur);
      advance' (note_dur +. gap));
    advance' long_gap);
  let song = Signal.render (result ()) |> Signal.gain 0.85 in
  Wav.save ~sampling_rate:44100 output_file [ song ]
