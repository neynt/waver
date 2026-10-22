(** Hyperbloom — a loopable hyperpop fragment in F# minor. Contrasts glitchy,
    pitched-up "terminally online" sections against stripped-back bells &
    breathy vowels for a nature pocket. 16 bars, 170 BPM, ~22s loop. *)
open Core

open Waver
module Compo = Composition.Make ()

let render output_file =
  let open Compo in
  let module C = Chiptune in
  let module I = Instrument in
  let bar = 16 in
  let song_bpm = 170. in
  bpm := song_bpm *. 4.;
  (* Chord helpers. *)
  let maj r = [ r; r + 4; r + 7 ] in
  let min_ r = [ r; r + 3; r + 7 ] in
  (* Parts. *)
  let pad_soft ~velo chord () =
    together
      (List.map chord ~f:(fun n () -> play bar (C.pxtone_organ ~velo n (bt bar))))
  in
  let pad_saw ~velo chord () =
    together
      (List.map chord ~f:(fun n () ->
         let s =
           C.supersaw ~velo n (bt bar) |> C.duck ~strength:0.7 ~bpm:song_bpm
         in
         play bar s))
  in
  let vowel ~synth midi () = play bar (synth midi (bt bar)) in
  let bell_top ~velo midi () =
    at 0 (C.bell ~velo midi (bt 8));
    at 8 (C.bell ~velo:(velo *. 0.75) midi (bt 8));
    advance bar
  in
  let pluck_arp ~velo chord () =
    let up = chord in
    let down = List.rev (List.drop up 1) in
    let cycle = up @ down in
    let n_cycle = List.length cycle in
    for i = 0 to 7 do
      at (i * 2) (C.pluck ~velo (List.nth_exn cycle (i % n_cycle) + 12) (bt 2))
    done;
    advance bar
  in
  let bass_4ofloor ~velo root () =
    let k = C.kick_808 ~velo () in
    let sub = C.sub_bass ~velo:(velo *. 0.9) root (bt' 0.9) in
    for i = 0 to 3 do
      at (i * 4) k;
      at (i * 4) sub
    done;
    advance bar
  in
  let bass_sub_long ~velo root () =
    at 0 (C.sub_bass ~velo root (bt' 6.0));
    at 8 (C.sub_bass ~velo root (bt' 6.0));
    advance bar
  in
  (* Vocal-chopped pseudo-vocal: retrigger "ee" at 8 chops per bar. *)
  let vox_stutter ~chops ~synth midi () =
    at 0 (C.vox_chop ~n:chops ~synth midi (bt bar));
    advance bar
  in
  (* Drum patterns. *)
  let drums_silent () = advance bar in
  let drums_awaken () =
    play_perc
      {| k . . h r . h . k . . h r . h h |}
      [ 'k', I.kick; 'r', C.rimshot; 'h', C.hat_closed ]
  in
  let drums_drop_a () =
    (* Four-on-floor with snare on 2 & 4, hat 16ths, open hat off last beat. *)
    play_perc
      {| k h s h k h s h k h s h k h s H |}
      [ 'k', I.kick; 's', C.snare; 'h', C.hat_closed; 'H', C.hat_open ]
  in
  let drums_drop_b () =
    play_perc
      {| k h c h k H c h k h c h k h c H |}
      [ 'k', I.kick; 'c', C.clap; 'h', C.hat_closed; 'H', C.hat_open ]
  in
  let drums_drop_fill () =
    (* Snare rush in the last half-bar to accent the loop-back. *)
    for i = 0 to 7 do
      at (i * 2) I.kick;
      at ((i * 2) + 1) C.hat_closed
    done;
    let positions = [ 8.; 9.; 10.; 11.; 12.; 12.75; 13.5; 14.; 14.5; 15.; 15.5; 15.8 ] in
    List.iter positions ~f:(fun t -> at' t (C.snare |> Signal.crop 0.04));
    advance bar
  in
  let drums_coda () =
    play_perc {| . . . . . . . . . . . . r . . . |} [ 'r', C.rimshot ]
  in
  (* Lead phrases. Each (Some midi, len_16ths) or (None, len). Lengths per
     bar sum to 16; phrase is 64 (4 bars). *)
  let lead ~instrument notes () =
    List.iter notes ~f:(fun (m_opt, len) ->
      Option.iter m_opt ~f:(fun m -> at 0 (instrument m (bt len)));
      advance len)
  in
  (* Pseudo-vocal hyperpop lead: vowel "ee" with pulse body + light distort for
     that chopped/autotuned character. *)
  let vocal_lead ~velo m d =
    let body = C.ee ~velo:(velo *. 1.1) ~breath:0.025 m d in
    let edge = C.pulse_lead ~velo:(velo *. 0.45) m d in
    Signal.add body edge |> C.distort 1.5
  in
  let ( !! ) m = Some m in
  let r n = None, n in
  (* Drop hook — tense, high, angular. Uses F#m pentatonic + chromatic slides. *)
  let ph_drop =
    [ !!78, 2; !!81, 2; !!85, 4; !!81, 3; !!78, 1; !!73, 4 ]
    (* F#m *)
    @ [ !!74, 2; !!78, 2; !!81, 2; !!83, 2; !!81, 2; !!78, 2; !!74, 4 ]
    (* D *)
    @ [ !!85, 2; !!81, 2; !!76, 2; !!73, 2; !!76, 2; !!81, 2; !!85, 4 ]
    (* A *)
    @ [ !!83, 2; !!80, 2; !!76, 2; !!80, 2; !!83, 4; r 4 ] (* E *)
  in
  (* Coda hook — simpler, longer notes, floats down. *)
  let ph_coda =
    [ !!78, 6; !!73, 2; !!69, 4; !!73, 4 ]
    (* F#m *)
    @ [ !!74, 6; !!78, 2; !!74, 4; r 4 ]
    (* D *)
    @ [ !!73, 4; !!76, 4; !!81, 4; !!78, 4 ]
    (* A *)
    @ [ !!76, 4; !!71, 2; !!68, 2; !!66, 4; r 4 ] (* E — resolves low, loops back to F# *)
  in
  (* Chord voicings (mid-register 3-note). *)
  let f_sharp_m = min_ 54 in
  (* F#4 A4 C#5 *)
  let d_maj = maj 50 in
  (* D4 F#4 A4 *)
  let a_maj = maj 52 in
  (* ~E4 A4 C#5 *)
  let e_maj = maj 52 in
  (* E4 G#4 B4 *)
  (* The four-chord progression — bass roots play lower octave. *)
  let prog =
    [ f_sharp_m, 42 (* F#2 *)
    ; d_maj, 38 (* D2 *)
    ; a_maj, 45 (* A2 *)
    ; e_maj, 40 (* E2 *)
    ]
  in
  let top_of ch = List.last_exn ch + 12 in
  (* ====================================================================== *)
  (* BARS 1-4  — SEED (pxtone pad + soft vowel, no drums).                   *)
  (* ====================================================================== *)
  List.iter prog ~f:(fun (ch, root) ->
    let top = top_of ch in
    together
      [ pad_soft ~velo:0.55 ch
      ; vowel ~synth:(C.oo ~breath:0.18 ~velo:0.4) top
      ; bass_sub_long ~velo:0.45 root
      ; drums_silent
      ]);
  (* ====================================================================== *)
  (* BARS 5-8  — AWAKEN (add bells, pluck arp, soft drums).                  *)
  (* ====================================================================== *)
  List.iter prog ~f:(fun (ch, root) ->
    let top = top_of ch in
    together
      [ pad_soft ~velo:0.6 ch
      ; vowel ~synth:(C.oo ~breath:0.12 ~velo:0.5) top
      ; bell_top ~velo:0.45 (top + 12)
      ; pluck_arp ~velo:0.35 ch
      ; bass_sub_long ~velo:0.6 root
      ; drums_awaken
      ]);
  (* ====================================================================== *)
  (* BARS 9-12 — DROP (hyperpop: supersaw, 808, sidechain, chopped vocal).   *)
  (* ====================================================================== *)
  let drop_drums = [ drums_drop_a; drums_drop_b; drums_drop_a; drums_drop_fill ] in
  let drop_chops = [ 8; 8; 16; 16 ] in
  together
    [ (fun () ->
        List.iteri (List.zip_exn prog drop_drums) ~f:(fun i ((ch, root), drums) ->
          let top = top_of ch in
          let n_chops = List.nth_exn drop_chops i in
          together
            [ pad_saw ~velo:0.55 ch
            ; vowel ~synth:(C.ee ~breath:0.03 ~velo:0.35) top
            ; bass_4ofloor ~velo:0.9 root
            ; vox_stutter
                ~chops:n_chops
                ~synth:(fun m d ->
                  C.ee ~breath:0.02 ~velo:0.55 m d |> C.bitcrush 6 |> C.distort 1.4)
                (top + 12)
            ; drums
            ]))
    ; lead ~instrument:(vocal_lead ~velo:0.9) ph_drop
    ];
  (* ====================================================================== *)
  (* BARS 13-16 — CODA (nature: bells, breathy vowel, pluck, coda hook).     *)
  (* ====================================================================== *)
  together
    [ (fun () ->
        List.iteri prog ~f:(fun i (ch, root) ->
          let top = top_of ch in
          let is_last = i = List.length prog - 1 in
          let parts =
            [ pad_soft ~velo:0.5 ch
            ; vowel ~synth:(C.ah ~breath:0.1 ~velo:0.4) top
            ; bell_top ~velo:0.4 (top + 12)
            ; pluck_arp ~velo:0.3 ch
            ; bass_sub_long ~velo:0.5 root
            ; (if is_last then drums_silent else drums_coda)
            ]
          in
          together parts))
    ; lead ~instrument:(fun m d -> C.bell ~velo:0.55 m d) ph_coda
    ];
  (* End exactly at bar 16 so the wav is a clean loop (16 bars × 16 16ths). *)
  end_here ();
  let song =
    Signal.render (result ())
    |> Signal.crop (bt (bar * bar))
    |> C.distort 1.25
    |> Signal.gain 0.85
  in
  Wav.save ~sampling_rate:44100 output_file [ song ]
