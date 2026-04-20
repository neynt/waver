(** Awakening — an 8-section chiptune-breakcore journey in A minor, with a
    mid-piece modulation up to C major, a plunge into F minor, a breakcore
    collapse, and a triumphant return to A minor. *)
open Core

open Waver
module Compo = Composition.Make ()

let render output_file =
  let open Compo in
  let module C = Chiptune in
  let module I = Instrument in
  let bar = 16 in
  bpm := 170. *. 4.;
  (* Chord helpers. *)
  let maj r = [ r; r + 4; r + 7 ] in
  let min_ r = [ r; r + 3; r + 7 ] in
  let _dom7 r = [ r; r + 4; r + 7; r + 10 ] in
  let add_octave ch = ch @ [ List.hd_exn ch + 12 ] in
  (* Parts — each is a (unit -> unit) that advances [bar] sixteenths. *)
  let pad ~velo chord () =
    together (List.map chord ~f:(fun n () -> play bar (C.pxtone_organ ~velo n (bt bar))))
  in
  let pad_root ~velo root () = play bar (C.pxtone_organ ~velo root (bt bar)) in
  let vowel ~synth midi () = play bar (synth midi (bt bar)) in
  let bass_sync ~velo root () =
    let hit = C.sub_bass ~velo root (bt' 1.8) in
    at 0 hit;
    at 6 hit;
    at 10 hit;
    at 14 hit;
    advance bar
  in
  let bass_half ~velo root () =
    let hit = C.sub_bass ~velo root (bt' 6.5) in
    at 0 hit;
    at 8 hit;
    advance bar
  in
  let bass_quarter ~velo root () =
    let hit = C.sub_bass ~velo root (bt' 3.6) in
    at 0 hit;
    at 4 hit;
    at 8 hit;
    at 12 hit;
    advance bar
  in
  let bass_octave ~velo root () =
    let low = C.sub_bass ~velo root (bt' 1.8) in
    let high = C.sub_bass ~velo:(velo *. 0.75) (root + 12) (bt' 1.6) in
    at 0 low;
    at 4 high;
    at 8 low;
    at 12 high;
    advance bar
  in
  let bass_growl ~velo root () =
    let hit = C.growl_bass ~velo root (bt' 1.8) in
    at 0 hit;
    at 4 hit;
    at 7 hit;
    at 10 hit;
    at 13 hit;
    advance bar
  in
  (* Arpeggio: 8 plucks per bar in an up-down pattern across the chord. *)
  let arp ~velo chord () =
    let up = chord in
    let down = List.rev (List.drop up 1) in
    let cycle = up @ down in
    let n_cycle = List.length cycle in
    let pattern = List.init 8 ~f:(fun i -> List.nth_exn cycle (i % n_cycle)) in
    List.iteri pattern ~f:(fun i note -> at (i * 2) (C.pluck ~velo (note + 12) (bt 2)));
    advance bar
  in
  (* Drum patterns — intentionally different feel per section. *)
  let drums_rim () =
    (* Stirring seed: just a rim click on beat 3. *)
    play_perc {| . . . . r . . . . . . . . . . . |} [ 'r', C.rimshot ]
  in
  let drums_rim_kick () =
    play_perc
      {| k . . . r . . . k . . . r . . h |}
      [ 'k', I.kick; 'r', C.rimshot; 'h', C.hat_closed ]
  in
  let drums_filling () =
    play_perc
      {| k . h . r . h k . . r . r h h h |}
      [ 'k', I.kick; 'r', C.rimshot; 'h', C.hat_closed ]
  in
  let drums_awaken_a () =
    (* Syncopated kick on 1 and "the and of 3". *)
    play_perc
      {| k . h . s . h k . . h . s . h h |}
      [ 'k', I.kick; 's', C.snare; 'h', C.hat_closed ]
  in
  let drums_awaken_b () =
    play_perc
      {| k h . k s h h . k h s k s . h h |}
      [ 'k', I.kick; 's', C.snare; 'h', C.hat_closed ]
  in
  let drums_awaken_fill () =
    play_perc
      {| k h s k s h s k s h s s k k k k |}
      [ 'k', I.kick; 's', C.snare; 'h', C.hat_closed ]
  in
  let drums_joy_a () =
    (* Dance-pop: kick + clap, open hats on off-beats. *)
    play_perc
      {| k . h c k . h c k . h c k . h c |}
      [ 'k', I.kick; 'c', C.clap; 'h', C.hat_closed ]
  in
  let drums_joy_b () =
    play_perc
      {| k . H c k . h c k H h c k . H c |}
      [ 'k', I.kick; 'c', C.clap; 'h', C.hat_closed; 'H', C.hat_open ]
  in
  let drums_joy_fill () =
    play_perc
      {| k c h c k c s c k c s c k s c s |}
      [ 'k', I.kick; 'c', C.clap; 's', C.snare; 'h', C.hat_closed ]
  in
  let drums_doubt () =
    (* Half-time — kick bar 1, snare bar 2 feel, very sparse. *)
    play_perc
      {| k . . . . . h . . . s . . . . . |}
      [ 'k', I.kick; 's', C.snare; 'h', C.hat_closed ]
  in
  let drums_doubt_sparse () =
    play_perc {| k . . . . . . . . . s . . . . . |} [ 'k', I.kick; 's', C.snare ]
  in
  let drums_break_a () =
    (* Breakcore: 32nd-note grid — positions 0..31 every half-16th. *)
    for i = 0 to 31 do
      let t = Float.of_int i *. 0.5 in
      if i % 8 = 0 then at' t I.kick;
      if i = 12 || i = 20 || i = 28 then at' t C.snare;
      if i % 2 = 0 then at' t C.hat_closed;
      if i >= 18 && i % 2 = 1 then at' t (C.snare |> Signal.crop 0.05);
      if i = 16 then at' t (C.hat_open |> Signal.gain 1.3)
    done;
    advance bar
  in
  let drums_break_roll () =
    (* Pitched-down kick pattern with rolling snares. *)
    let low_kick = I.kick |> Signal.freq 0.6 |> Signal.gain 1.2 in
    for i = 0 to 31 do
      let t = Float.of_int i *. 0.5 in
      if i % 4 = 0 then at' t low_kick;
      if i % 3 = 0 then at' t C.rimshot;
      if i > 16 then at' t (C.snare |> Signal.crop 0.03)
    done;
    advance bar
  in
  let drums_break_buildup () =
    (* Build density — snare rolls accelerate. *)
    let mini = C.snare |> Signal.crop 0.04 in
    let positions =
      [ 0.; 3.; 6.; 8.; 9.5; 11.; 12.; 12.75; 13.5; 14.; 14.5; 15.; 15.25; 15.5; 15.75 ]
    in
    List.iter positions ~f:(fun t -> at' t mini);
    at' 15.9 I.kick;
    advance bar
  in
  let drums_triumph_a () =
    (* Big, kick-heavy, open hats. *)
    play_perc
      {| k h H c s h H h k h s k s h H k |}
      [ 'k', I.kick; 'c', C.clap; 's', C.snare; 'h', C.hat_closed; 'H', C.hat_open ]
  in
  let drums_triumph_b () =
    play_perc
      {| k . H k s h H h k H s k s h H h |}
      [ 'k', I.kick; 's', C.snare; 'h', C.hat_closed; 'H', C.hat_open ]
  in
  let drums_triumph_fill () =
    play_perc
      {| k c h c s c h c k c s c k s s k |}
      [ 'k', I.kick; 'c', C.clap; 's', C.snare; 'h', C.hat_closed ]
  in
  let drums_peace () = play_perc {| . . . . . . . . k . . . . . . . |} [ 'k', I.kick ] in
  (* Lead phrases. Each tuple is (Some midi / None for rest, length in 16ths).
     Lengths sum to [bar * n_bars]. Rhythms mix dotted eighths (3), eighths
     (2), sixteenths (1), half notes (8) — intentionally not a regular grid. *)
  let lead_phrase ~instrument notes () =
    List.iter notes ~f:(fun (m_opt, len) ->
      Option.iter m_opt ~f:(fun m -> at 0 (instrument m (bt len)));
      advance len)
  in
  (* Helper: transpose a phrase up by [n] semitones. *)
  let transpose n = List.map ~f:(fun (m_opt, l) -> Option.map m_opt ~f:(( + ) n), l) in
  let ( !! ) m = Some m in
  let r n = None, n in
  (* Phrases for Awakening (bars 13-24, 12 bars = 3 x 4). *)
  let ph_awaken_1 =
    [ !!76, 3; !!79, 1; !!76, 4; !!72, 3; !!69, 1; !!72, 4 ]
    @ [ !!74, 2; !!72, 2; !!69, 4; !!65, 3; !!69, 1; !!72, 4 ]
    @ [ !!76, 3; !!77, 1; !!79, 4; !!81, 3; !!79, 1; !!76, 4 ]
    @ [ !!74, 6; !!72, 2; !!69, 8 ]
  in
  let ph_awaken_2 =
    [ !!81, 2; !!79, 2; !!76, 4; !!72, 3; !!76, 1; !!79, 4 ]
    @ [ !!77, 2; !!76, 2; !!74, 4; !!72, 3; !!74, 1; !!72, 4 ]
    @ [ !!76, 3; !!79, 1; !!81, 4; !!79, 3; !!76, 1; !!72, 4 ]
    @ [ !!74, 4; !!72, 4; !!69, 8 ]
  in
  let ph_awaken_3 =
    [ !!69, 2; !!72, 2; !!76, 3; !!79, 1; !!81, 4; !!79, 4 ]
    @ [ !!77, 3; !!76, 1; !!72, 4; !!74, 3; !!72, 1; !!69, 4 ]
    @ [ !!76, 2; !!79, 2; !!81, 4; !!84, 3; !!81, 1; !!79, 4 ]
    @ [ !!77, 4; !!74, 4; !!72, 6; r 2 ]
  in
  (* Joy phrases (bars 25-36, C major, 3 x 4). Bright, major, more syncopation. *)
  let ph_joy_1 =
    [ !!72, 2; !!76, 2; !!79, 3; !!84, 1; !!79, 4; !!76, 4 ]
    @ [ !!77, 3; !!76, 1; !!74, 4; !!72, 4; !!69, 4 ]
    @ [ !!72, 2; !!74, 2; !!76, 3; !!79, 1; !!77, 4; !!72, 4 ]
    @ [ !!74, 4; !!72, 4; !!71, 4; !!72, 4 ]
  in
  let ph_joy_2 =
    [ !!84, 3; !!83, 1; !!79, 2; !!76, 2; !!79, 4; !!84, 4 ]
    @ [ !!83, 2; !!79, 2; !!76, 3; !!72, 1; !!74, 4; !!76, 4 ]
    @ [ !!77, 2; !!76, 2; !!74, 4; !!72, 2; !!74, 2; !!76, 4 ]
    @ [ !!79, 3; !!76, 1; !!72, 4; !!69, 8 ]
  in
  let ph_joy_3 =
    [ !!79, 2; !!84, 2; !!83, 2; !!81, 2; !!79, 4; !!76, 4 ]
    @ [ !!84, 3; !!83, 1; !!81, 4; !!79, 3; !!77, 1; !!76, 4 ]
    @ [ !!79, 2; !!76, 2; !!74, 2; !!76, 2; !!72, 4; !!69, 4 ]
    @ [ !!72, 4; r 2; !!72, 2; !!76, 4; !!79, 4 ]
  in
  (* Doubt phrases (bars 37-48, Fm, 3 x 4). Slow, sighing, descending. *)
  let ph_doubt_1 =
    [ !!77, 4; !!75, 4; !!72, 8 ]
    @ [ !!68, 4; !!67, 4; !!65, 8 ]
    @ [ !!70, 4; !!68, 4; !!65, 4; !!63, 4 ]
    @ [ !!65, 6; r 2; !!60, 8 ]
  in
  let ph_doubt_2 =
    [ !!75, 3; !!72, 1; !!70, 4; !!68, 8 ]
    @ [ !!72, 4; !!68, 4; !!65, 4; !!63, 4 ]
    @ [ !!68, 2; !!65, 2; !!63, 4; !!65, 8 ]
    @ [ !!72, 6; !!68, 2; !!65, 8 ]
  in
  let ph_doubt_3 =
    [ !!80, 4; !!77, 4; !!75, 8 ]
    @ [ !!72, 3; !!70, 1; !!68, 4; !!72, 8 ]
    @ [ !!75, 4; !!72, 4; !!70, 2; !!68, 2; !!65, 4 ]
    @ [ !!63, 8; !!60, 8 ]
  in
  (* Triumph phrases (bars 57-76, Am, 5 x 4). Strong, rising, heroic. *)
  let ph_triumph_1 =
    [ !!69, 2; !!72, 2; !!76, 3; !!81, 1; !!76, 4; !!72, 4 ]
    @ [ !!77, 2; !!76, 2; !!74, 2; !!72, 2; !!69, 4; !!72, 4 ]
    @ [ !!76, 3; !!79, 1; !!81, 4; !!79, 2; !!76, 2; !!72, 4 ]
    @ [ !!81, 4; !!76, 4; !!72, 4; !!69, 4 ]
  in
  let ph_triumph_2 =
    [ !!81, 3; !!84, 1; !!88, 2; !!84, 2; !!81, 4; !!76, 4 ]
    @ [ !!77, 3; !!81, 1; !!79, 2; !!77, 2; !!74, 4; !!72, 4 ]
    @ [ !!76, 2; !!79, 2; !!81, 3; !!84, 1; !!81, 4; !!79, 4 ]
    @ [ !!81, 6; !!76, 2; !!72, 4; !!69, 4 ]
  in
  (* Chord voicings. *)
  let am_lo = min_ 45 in
  (* A2 *)
  let am = min_ 57 in
  let am_big = add_octave am in
  let f_ = maj 53 in
  let f_big = add_octave f_ in
  let c_ = maj 48 in
  let c_big = add_octave c_ in
  let c_hi = maj 60 in
  let g_ = maj 55 in
  let g_hi = maj 55 in
  let e_ = maj 52 in
  let dm = min_ 50 in
  let _em = min_ 52 in
  let fm = min_ 53 in
  let db = maj 49 in
  let ab = maj 56 in
  let eb = maj 51 in
  let cm = min_ 48 in
  let bb = maj 46 in
  let bbm = min_ 46 in
  let _ = am_lo in
  (* Tag each chord with a bass root. *)
  (* ====================================================================== *)
  (* SECTION 1: SOLITUDE  (bars 1-8, Am — sparse, lonely, no drums).         *)
  (* ====================================================================== *)
  let solitude =
    [ am, 45, 69
    ; am, 45, 69
    ; f_, 41, 65
    ; f_, 41, 65
    ; am, 45, 69
    ; am, 45, 69
    ; e_, 40, 64
    ; e_, 40, 64
    ]
  in
  List.iter solitude ~f:(fun (_, bass_root, top) ->
    together
      [ pad_root ~velo:0.55 bass_root; vowel ~synth:(C.oo ~breath:0.15 ~velo:0.45) top ]);
  (* ====================================================================== *)
  (* SECTION 2: STIRRING  (bars 9-20, Am — 3 x 4 cycle, builds layers).      *)
  (* ====================================================================== *)
  let stirring_prog =
    [ am, 45
    ; f_, 41
    ; dm, 38
    ; e_, 40
    ; am, 45
    ; f_, 41
    ; dm, 38
    ; e_, 40
    ; am, 45
    ; f_, 41
    ; c_, 36
    ; e_, 40
    ]
  in
  List.iteri stirring_prog ~f:(fun i (ch, root) ->
    let top = List.last_exn ch + 12 in
    let parts = [ pad ~velo:0.7 ch; vowel ~synth:(C.oo ~breath:0.1 ~velo:0.5) top ] in
    let parts = if i >= 0 then parts @ [ arp ~velo:0.4 ch ] else parts in
    let parts = if i >= 4 then parts @ [ bass_sync ~velo:0.6 root ] else parts in
    let parts =
      if i < 4
      then parts @ [ drums_rim ]
      else if i < 8
      then parts @ [ drums_rim_kick ]
      else parts @ [ drums_filling ]
    in
    together parts);
  (* ====================================================================== *)
  (* SECTION 3: AWAKENING  (bars 21-32, Am + pulse lead + full drums).       *)
  (* ====================================================================== *)
  let awakening_prog =
    [ am, 45
    ; f_, 41
    ; c_, 36
    ; g_, 43
    ; am, 45
    ; f_, 41
    ; dm, 38
    ; e_, 40
    ; am, 45
    ; f_, 41
    ; c_, 36
    ; e_, 40
    ]
  in
  let awakening_drums =
    [ drums_awaken_a
    ; drums_awaken_a
    ; drums_awaken_b
    ; drums_awaken_a
    ; drums_awaken_a
    ; drums_awaken_b
    ; drums_awaken_a
    ; drums_awaken_b
    ; drums_awaken_b
    ; drums_awaken_a
    ; drums_awaken_b
    ; drums_awaken_fill
    ]
  in
  let awakening_phrase_notes = List.concat [ ph_awaken_1; ph_awaken_2; ph_awaken_3 ] in
  (* Run the 12-bar progression + drums + pad + vocal backing in parallel with
     the 12-bar pulse lead — `together` tracks their max advance. *)
  together
    [ (fun () ->
        List.iter
          (List.zip_exn awakening_prog awakening_drums)
          ~f:(fun ((ch, root), drums) ->
            let top = List.last_exn ch + 12 in
            together
              [ pad ~velo:0.6 ch
              ; vowel ~synth:(C.ee ~breath:0.03 ~velo:0.3) top
              ; bass_octave ~velo:0.8 root
              ; drums
              ]))
    ; lead_phrase ~instrument:C.pulse_lead awakening_phrase_notes
    ];
  (* ====================================================================== *)
  (* SECTION 4: JOY  (bars 33-44, C major — modulate up, dance feel).        *)
  (* ====================================================================== *)
  let joy_prog =
    [ c_hi, 36
    ; g_hi, 43
    ; am, 45
    ; f_, 41
    ; c_hi, 36
    ; g_hi, 43
    ; f_, 41
    ; g_hi, 43
    ; c_hi, 36
    ; g_hi, 43
    ; am, 45
    ; g_hi, 43
    ]
  in
  let joy_drums =
    [ drums_joy_a
    ; drums_joy_a
    ; drums_joy_b
    ; drums_joy_b
    ; drums_joy_a
    ; drums_joy_a
    ; drums_joy_b
    ; drums_joy_b
    ; drums_joy_b
    ; drums_joy_a
    ; drums_joy_b
    ; drums_joy_fill
    ]
  in
  let joy_phrase_notes = List.concat [ ph_joy_1; ph_joy_2; ph_joy_3 ] in
  together
    [ (fun () ->
        List.iter (List.zip_exn joy_prog joy_drums) ~f:(fun ((ch, root), drums) ->
          let top = List.last_exn ch + 12 in
          together
            [ pad ~velo:0.75 ch
            ; vowel ~synth:(C.ah ~breath:0.04 ~velo:0.6) top
            ; bass_octave ~velo:0.9 root
            ; drums
            ]))
    ; lead_phrase ~instrument:C.pulse_lead joy_phrase_notes
    ];
  (* ====================================================================== *)
  (* SECTION 5: DOUBT  (bars 45-56, Fm — dark modulation, slow feel).        *)
  (* ====================================================================== *)
  let doubt_prog =
    [ fm, 41
    ; db, 37
    ; ab, 44
    ; eb, 39
    ; fm, 41
    ; db, 37
    ; bbm, 46
    ; eb, 39
    ; fm, 41
    ; db, 37
    ; cm, 36
    ; bb, 46
    ]
  in
  let doubt_phrase_notes = List.concat [ ph_doubt_1; ph_doubt_2; ph_doubt_3 ] in
  together
    [ (fun () ->
        List.iteri doubt_prog ~f:(fun i (ch, root) ->
          let top = List.last_exn ch + 12 in
          let drums = if i < 4 then drums_doubt_sparse else drums_doubt in
          together
            [ pad ~velo:0.55 ch
            ; vowel ~synth:(C.uh ~breath:0.08 ~velo:0.5) top
            ; bass_half ~velo:0.55 root
            ; drums
            ]))
    ; lead_phrase ~instrument:C.pulse_lead doubt_phrase_notes
    ];
  (* ====================================================================== *)
  (* SECTION 6: BREAKING  (bars 57-64, E pedal — breakcore chaos).           *)
  (* ====================================================================== *)
  let break_drums =
    [ drums_break_a
    ; drums_break_roll
    ; drums_break_a
    ; drums_break_roll
    ; drums_break_a
    ; drums_break_roll
    ; drums_break_a
    ; drums_break_buildup
    ]
  in
  List.iter break_drums ~f:(fun drums ->
    together
      [ pad ~velo:0.45 [ 40; 47; 52 ]
      ; (* E2 B2 E3 — pedal/5th stack *)
        vowel ~synth:(C.eh ~breath:0.12 ~velo:0.4) 76
      ; bass_growl ~velo:0.9 28
      ; drums
      ]);
  (* Crash at the triumph entry. *)
  at 0 C.crash;
  (* ====================================================================== *)
  (* SECTION 7: TRIUMPH  (bars 65-80, Am — full band + harmony lead + bells).*)
  (* ====================================================================== *)
  let triumph_prog =
    [ am_big, 33
    ; f_big, 29
    ; dm, 38
    ; e_, 40
    ; am_big, 33
    ; f_big, 29
    ; c_big, 36
    ; g_, 43
    ; am_big, 33
    ; f_big, 29
    ; dm, 38
    ; e_, 40
    ; am_big, 33
    ; f_big, 29
    ; c_big, 36
    ; g_, 43
    ]
  in
  let triumph_drums =
    [ drums_triumph_a
    ; drums_triumph_a
    ; drums_triumph_b
    ; drums_triumph_a
    ; drums_triumph_a
    ; drums_triumph_b
    ; drums_triumph_a
    ; drums_triumph_fill
    ; drums_triumph_b
    ; drums_triumph_a
    ; drums_triumph_b
    ; drums_triumph_a
    ; drums_triumph_a
    ; drums_triumph_b
    ; drums_triumph_a
    ; drums_triumph_fill
    ]
  in
  let triumph_phrase_notes =
    List.concat [ ph_triumph_1; ph_triumph_2; ph_triumph_1; ph_triumph_2 ]
  in
  together
    [ (fun () ->
        List.iter (List.zip_exn triumph_prog triumph_drums) ~f:(fun ((ch, root), drums) ->
          let top = List.last_exn ch + 12 in
          together
            [ pad ~velo:0.85 ch
            ; vowel ~synth:(C.ee ~breath:0.04 ~velo:0.7) top
            ; bass_quarter ~velo:1.0 root
            ; drums
            ; (fun () ->
                at 0 (C.bell ~velo:0.5 (List.last_exn ch + 12) (bt 8));
                at 8 (C.bell ~velo:0.5 (List.nth_exn ch 1 + 12) (bt 8));
                advance bar)
            ]))
    ; lead_phrase ~instrument:C.pulse_lead triumph_phrase_notes
    ; lead_phrase
        ~instrument:(fun m d -> C.pulse_lead ~velo:0.5 m d)
        (transpose 4 triumph_phrase_notes)
    ];
  (* ====================================================================== *)
  (* SECTION 8: PEACE  (bars 81-88, Am — fade to silence).                   *)
  (* ====================================================================== *)
  let peace_prog = [ am, 45; am, 45; f_, 41; f_, 41; am, 45; am, 45; am, 45; am, 45 ] in
  List.iteri peace_prog ~f:(fun i (ch, _root) ->
    let fade = 1. -. (Float.of_int i *. 0.12) in
    let top = List.last_exn ch + 12 in
    let parts =
      [ pad ~velo:(0.6 *. fade) ch
      ; vowel ~synth:(C.oo ~breath:0.15 ~velo:(0.5 *. fade)) top
      ]
    in
    let parts = if i < 3 then parts @ [ drums_peace ] else parts in
    together parts);
  let song = Signal.render (result ()) |> C.distort 1.4 |> Signal.gain 0.82 in
  Wav.save ~sampling_rate:44100 output_file [ song ]
