open Core
open Waver
module Compo = Composition.Make ()

let render output_file =
  let open Compo in
  let module C = Chiptune in
  let module I = Instrument in
  (* Treat each composer "beat" as one 16th note at 170 BPM. *)
  let musical_bpm = 170. in
  bpm := musical_bpm *. 4.;
  (* i VI III VII in A minor: Am, F, C, G. Voicings stay in the same register. *)
  let am = [ 57; 60; 64 ] in
  let f_ = [ 53; 57; 60 ] in
  let c_ = [ 48; 55; 60 ] in
  let g_ = [ 50; 55; 59 ] in
  let progression = [ am; f_; c_; g_ ] in
  let bass_roots = [ 33; 29; 36; 31 ] in
  (* One bar = 16 sixteenth units. *)
  let bar = 16 in
  let pad ~gain_ ch () =
    together
      (List.map ch ~f:(fun m () -> play bar (C.pxtone_organ ~velo:gain_ m (bt bar))))
  in
  let vocals ~gain_ ch () =
    let top = List.last_exn ch + 12 in
    play bar (C.vocaloid_ah ~velo:gain_ top (bt bar))
  in
  let bass ~gain_ root () =
    let hit = C.sub_bass ~velo:gain_ root (bt' 1.6) in
    at 0 hit;
    at 6 hit;
    at 10 hit;
    at 14 hit;
    advance bar
  in
  let drums_basic () =
    play_perc
      {| k . h . s . h . k . h . s . h . |}
      [ 'k', I.kick; 's', C.snare; 'h', C.hat_closed ]
  in
  let drums_groove () =
    play_perc
      {| k . h k s h h . k h s k s . h h |}
      [ 'k', I.kick; 's', C.snare; 'h', C.hat_closed ]
  in
  let drums_open () =
    play_perc
      {| k . H k s h H . k h s k s h H h |}
      [ 'k', I.kick; 's', C.snare; 'h', C.hat_closed; 'H', C.hat_open ]
  in
  let drums_breakcore () =
    (* 32-step roll pattern — 1/32nd subdivision inside a bar. The play_perc
       step unit here is half a 16th, so we advance 2 grid-units per comp beat. *)
    let half = C.snare |> Signal.crop 0.04 in
    let tri = C.hat_closed in
    for i = 0 to 31 do
      let t = Float.of_int i *. 0.5 in
      if i % 8 = 0 then at' t I.kick;
      if i = 12 || i = 28 then at' t C.snare;
      if i % 2 = 0 then at' t tri;
      if i >= 20 && i % 2 = 1 then at' t half
    done;
    advance bar
  in
  let lead_phrase notes () =
    List.iter notes ~f:(fun (m, len) ->
      at 0 (C.pulse_lead m (bt len));
      advance len)
  in
  (* Four 2-bar lead phrases in A minor pentatonic-ish territory. *)
  let ph1 = [ 76, 2; 79, 2; 81, 4; 79, 2; 76, 2; 72, 4 ] in
  let ph2 = [ 77, 2; 72, 2; 69, 4; 72, 2; 74, 2; 76, 4 ] in
  let ph3 = [ 72, 2; 76, 2; 79, 4; 76, 2; 72, 2; 67, 4 ] in
  let ph4 = [ 79, 2; 76, 2; 72, 4; 74, 2; 76, 2; 79, 4 ] in
  let sections = progression in
  let roots = bass_roots in
  (* Bars 1-4: organ + vocals intro. *)
  List.iter2_exn sections roots ~f:(fun ch _root ->
    together [ pad ~gain_:0.9 ch; vocals ~gain_:0.7 ch ]);
  (* Bars 5-8: add bass + basic drums — the groove settles in. *)
  List.iter2_exn sections roots ~f:(fun ch root ->
    together [ pad ~gain_:0.8 ch; bass ~gain_:0.9 root; drums_basic ]);
  (* Bar 9: crash + full drop. Place the crash parallel to bars 9-12 below. *)
  at 0 C.crash;
  (* Bars 9-12: full drop — pad, bass, drums_groove, lead. *)
  let drop_section phrases =
    List.iter2_exn (List.zip_exn sections roots) phrases ~f:(fun (ch, root) phrase ->
      together
        [ pad ~gain_:0.7 ch; bass ~gain_:0.9 root; drums_groove; lead_phrase phrase ])
  in
  drop_section [ ph1; ph2; ph3; ph4 ];
  (* Bars 13-14: breakcore stutter break — stripped to drums + pad. *)
  together [ pad ~gain_:0.5 am; drums_breakcore ];
  together [ pad ~gain_:0.5 f_; drums_breakcore ];
  (* Bars 15-18: climax — open hats, vocals on top, louder everything. *)
  List.iter2_exn
    (List.zip_exn sections roots)
    [ ph3; ph4; ph1; ph2 ]
    ~f:(fun (ch, root) phrase ->
      together
        [ pad ~gain_:0.9 ch
        ; vocals ~gain_:0.6 ch
        ; bass ~gain_:1.0 root
        ; drums_open
        ; lead_phrase phrase
        ]);
  (* Bars 19-20: outro — pad + vocals fading. *)
  together [ pad ~gain_:0.7 am; vocals ~gain_:0.5 am ];
  together [ pad ~gain_:0.4 f_; vocals ~gain_:0.3 f_ ];
  let song = Signal.render (result ()) |> C.distort 1.6 |> Signal.gain 0.85 in
  Wav.save ~sampling_rate:44100 output_file [ song ]
