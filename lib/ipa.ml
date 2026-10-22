(** Rough IPA-ish singing synthesis. Each phone carries its own renderer and
    an [overlap] value (seconds it should overlap the next phone) so [sing]
    can glue phones together with some co-articulation. *)
open Base

let sr = 44100

(* Formant sets (center Hz, bandwidth Hz, amp). *)
(* Vowels — same presets as chiptune, duplicated here so the sing vowels don't
   pick up chiptune's standalone envelope. *)
let ah_formants =
  [ 730., 90., 1.0; 1090., 110., 0.85; 2440., 130., 0.55; 3400., 160., 0.25 ]

let ee_formants =
  [ 270., 70., 1.0; 2290., 110., 1.0; 3010., 130., 0.6; 3800., 160., 0.2 ]

let oo_formants = [ 300., 80., 1.0; 870., 90., 0.7; 2240., 120., 0.3; 3200., 140., 0.1 ]
let oh_formants = [ 570., 80., 1.0; 840., 90., 0.8; 2410., 120., 0.4; 3300., 140., 0.1 ]
let eh_formants = [ 530., 80., 1.0; 1840., 110., 0.8; 2480., 130., 0.5; 3500., 160., 0.2 ]
let uh_formants = [ 640., 80., 1.0; 1190., 100., 0.6; 2390., 130., 0.3; 3200., 140., 0.1 ]

(* Voiced consonants. *)
let m_formants = [ 280., 60., 1.0; 1100., 180., 0.15; 2250., 220., 0.05 ]
let n_formants = [ 280., 60., 1.0; 1500., 180., 0.25; 2300., 220., 0.08 ]
let l_formants = [ 360., 90., 1.0; 1300., 120., 0.7; 2700., 140., 0.5 ]

let r_formants =
  [ 400., 90., 1.0; 1060., 110., 0.65; 1600., 140., 0.55; 3400., 180., 0.25 ]

let w_formants = [ 300., 80., 1.0; 750., 90., 0.6; 2400., 130., 0.2 ]
let y_formants = [ 270., 70., 1.0; 2290., 110., 0.9; 3010., 130., 0.4 ]

(* Short, concatenation-friendly envelopes. Attack + release stay under ~50 ms
   so phones can overlap cleanly without long fade tails stacking up. *)
let env_vowel dur =
  Signal.pwlin [ 0.025, 1.; Float.max 0.005 (dur -. 0.05), 1.; 0.025, 0. ]

let env_voiced dur =
  Signal.pwlin [ 0.015, 1.; Float.max 0.005 (dur -. 0.035), 1.; 0.02, 0. ]

(* Slow ramp on fricatives — sibilants attack-from-zero too quickly read as a
   percussive shock; 30 ms attack lets them "swell" in. *)
let env_fric dur =
  Signal.pwlin [ 0.03, 1.; Float.max 0.005 (dur -. 0.07), 1.; 0.04, 0. ]

let env_stop dur =
  Signal.pwlin [ 0.002, 1.; Float.max 0.003 (dur -. 0.015), 1.; 0.013, 0. ]

(* Additive formant-based voiced tone. Used for vowels and voiced consonants. *)
let voiced_tone
      ?(n_harmonics = 32)
      ?(breath = 0.015)
      ?(vib_depth = 0.005)
      ?(vib_rate = 5.0)
      ~gain_mul
      ~formants
      ~velo
      ~env
      midi
      dur
  =
  let f0 = Temperament.equal 440. midi in
  let formant_gain freq =
    List.fold formants ~init:0. ~f:(fun acc (fc, bw, amp) ->
      let d = freq -. fc in
      acc +. (amp *. bw *. bw /. ((d *. d) +. (bw *. bw))))
  in
  let harmonics =
    List.init n_harmonics ~f:(fun i ->
      let n = i + 1 in
      let fn = f0 *. Float.of_int n in
      let w = formant_gain fn /. Float.of_int n in
      Signal.sine fn |> Signal.gain w)
    |> List.reduce_balanced ~f:Signal.add
    |> Option.value ~default:Signal.zero
  in
  let body =
    if Float.(breath <= 0.)
    then harmonics
    else Signal.add harmonics (Signal.unpure_noise |> Signal.gain breath)
  in
  let tau = 2. *. Signal.pi in
  let modulator =
    Signal.add
      Signal.ramp
      (Signal.cosine vib_rate |> Signal.gain (vib_depth /. (tau *. vib_rate)))
  in
  Signal.phase_mod modulator body
  |> Signal.mul env
  |> Signal.gain (gain_mul *. velo)
  |> Signal.crop dur

type phone =
  { render : midi:int -> dur:float -> Signal.t
  ; overlap : float
  }

let make_vowel ~formants =
  { render =
      (fun ~midi ~dur ->
        voiced_tone
          ~gain_mul:0.25
          ~formants
          ~velo:0.9
          ~env:(env_vowel dur)
          midi
          dur)
  ; overlap = 0.06
  }

let vowel_ah = make_vowel ~formants:ah_formants
let vowel_ee = make_vowel ~formants:ee_formants
let vowel_oo = make_vowel ~formants:oo_formants
let vowel_oh = make_vowel ~formants:oh_formants
let vowel_eh = make_vowel ~formants:eh_formants
let vowel_uh = make_vowel ~formants:uh_formants

let make_voiced_consonant ~formants ~velo ~overlap =
  { render =
      (fun ~midi ~dur ->
        voiced_tone
          ~gain_mul:0.22
          ~breath:0.01
          ~formants
          ~velo
          ~env:(env_voiced dur)
          midi
          dur)
  ; overlap
  }

(* Nasals — post-filter the formant synth with a gentle lowpass so the upper
   formants get damped the way a real nasal cavity damps them. Cutoff at the
   F2 region: 'm' is darker (lips closed) so cuts lower. *)
let make_nasal ~formants ~lp_freq ~velo =
  let base = make_voiced_consonant ~formants ~velo ~overlap:0.05 in
  { render =
      (fun ~midi ~dur ->
        base.render ~midi ~dur |> Signal.biquad_lowpass sr ~freq:lp_freq ~q:0.6)
  ; overlap = base.overlap
  }

let nasal_m = make_nasal ~formants:m_formants ~lp_freq:900. ~velo:1.6
let nasal_n = make_nasal ~formants:n_formants ~lp_freq:1300. ~velo:1.6
let liquid_l = make_voiced_consonant ~formants:l_formants ~velo:0.85 ~overlap:0.05
let liquid_r = make_voiced_consonant ~formants:r_formants ~velo:0.85 ~overlap:0.05
let glide_w = make_voiced_consonant ~formants:w_formants ~velo:0.8 ~overlap:0.05
let glide_y = make_voiced_consonant ~formants:y_formants ~velo:0.8 ~overlap:0.05

(* Time-varying formant tone — interpolates linearly from [formants_start] at
   t=0 to [formants_end] at t=dur. Used for true glides like /j/, /w/ where the
   acoustic signature is a continuous F2 sweep, not amplitude crossfade. *)
let voiced_glide
      ?(n_harmonics = 32)
      ?(breath = 0.01)
      ?(vib_depth = 0.003)
      ?(vib_rate = 5.0)
      ~gain_mul
      ~formants_start
      ~formants_end
      ~velo
      ~env
      midi
      dur
  =
  let f0 = Temperament.equal 440. midi in
  let formant_gain_of formants freq =
    List.fold formants ~init:0. ~f:(fun acc (fc, bw, amp) ->
      let d = freq -. fc in
      acc +. (amp *. bw *. bw /. ((d *. d) +. (bw *. bw))))
  in
  let harmonics =
    List.init n_harmonics ~f:(fun i ->
      let n = i + 1 in
      let fn = f0 *. Float.of_int n in
      let w_a = formant_gain_of formants_start fn /. Float.of_int n in
      let w_b = formant_gain_of formants_end fn /. Float.of_int n in
      let weight =
        Signal.create
          (fun t ->
            let p = Float.max 0. (Float.min 1. (t /. dur)) in
            ((1. -. p) *. w_a) +. (p *. w_b))
          dur
      in
      Signal.sine fn |> Signal.mul weight)
    |> List.reduce_balanced ~f:Signal.add
    |> Option.value ~default:Signal.zero
  in
  let body =
    if Float.(breath <= 0.)
    then harmonics
    else Signal.add harmonics (Signal.unpure_noise |> Signal.gain breath)
  in
  let tau = 2. *. Signal.pi in
  let modulator =
    Signal.add
      Signal.ramp
      (Signal.cosine vib_rate |> Signal.gain (vib_depth /. (tau *. vib_rate)))
  in
  Signal.phase_mod modulator body
  |> Signal.mul env
  |> Signal.gain (gain_mul *. velo)
  |> Signal.crop dur

(* /j/-into-vowel: starts at y formants, sweeps to the next vowel's formants.
   Use [Ipa.glide_y_to oo_formants] right before [vowel_oo] for "you". *)
let glide_y_to target_formants =
  { render =
      (fun ~midi ~dur ->
        voiced_glide
          ~gain_mul:0.22
          ~formants_start:y_formants
          ~formants_end:target_formants
          ~velo:0.85
          ~env:(env_voiced dur)
          midi
          dur)
  ; overlap = 0.02
  }

(* /w/-into-vowel: same idea, w formants sweep to the next vowel. *)
let glide_w_to target_formants =
  { render =
      (fun ~midi ~dur ->
        voiced_glide
          ~gain_mul:0.22
          ~formants_start:w_formants
          ~formants_end:target_formants
          ~velo:0.85
          ~env:(env_voiced dur)
          midi
          dur)
  ; overlap = 0.02
  }

(* h — breath-shaped noise. Lowpassed & gentle so it reads as "aspiration" into
   the following vowel rather than a hiss. *)
(* Unvoiced phones are biquad-filtered noise. Bandpass has peak gain = Q at
   centre, so the base [gain] is small; combined with envelope and Q the per-
   phone peak ends up well below the vowels. *)

let glide_h =
  { render =
      (fun ~midi:_ ~dur ->
        Signal.unpure_noise
        |> Signal.biquad_lowpass sr ~freq:1400. ~q:0.7
        |> Signal.mul (env_fric dur)
        |> Signal.gain 0.04
        |> Signal.crop dur)
  ; overlap = 0.06
  }

let fric_s =
  { render =
      (fun ~midi:_ ~dur ->
        Signal.unpure_noise
        |> Signal.biquad_bandpass sr ~freq:6500. ~q:1.8
        |> Signal.mul (env_fric dur)
        |> Signal.gain 0.025
        |> Signal.crop dur)
  ; overlap = 0.01
  }

let fric_f =
  { render =
      (fun ~midi:_ ~dur ->
        Signal.unpure_noise
        |> Signal.biquad_bandpass sr ~freq:4200. ~q:0.7
        |> Signal.mul (env_fric dur)
        |> Signal.gain 0.035
        |> Signal.crop dur)
  ; overlap = 0.01
  }

let fric_sh =
  { render =
      (fun ~midi:_ ~dur ->
        Signal.unpure_noise
        |> Signal.biquad_bandpass sr ~freq:2800. ~q:1.0
        |> Signal.mul (env_fric dur)
        |> Signal.gain 0.04
        |> Signal.crop dur)
  ; overlap = 0.01
  }

let stop_p =
  { render =
      (fun ~midi:_ ~dur ->
        Signal.unpure_noise
        |> Signal.biquad_lowpass sr ~freq:900. ~q:0.7
        |> Signal.mul (env_stop dur)
        |> Signal.gain 0.07
        |> Signal.crop dur)
  ; overlap = 0.005
  }

let stop_t =
  { render =
      (fun ~midi:_ ~dur ->
        Signal.unpure_noise
        |> Signal.biquad_bandpass sr ~freq:4500. ~q:2.0
        |> Signal.mul (env_stop dur)
        |> Signal.gain 0.06
        |> Signal.crop dur)
  ; overlap = 0.005
  }

let stop_k =
  { render =
      (fun ~midi:_ ~dur ->
        Signal.unpure_noise
        |> Signal.biquad_bandpass sr ~freq:1800. ~q:1.4
        |> Signal.mul (env_stop dur)
        |> Signal.gain 0.07
        |> Signal.crop dur)
  ; overlap = 0.005
  }

let silence =
  { render = (fun ~midi:_ ~dur -> Signal.create (fun _ -> 0.) dur); overlap = 0. }

type note =
  { phone : phone
  ; midi : int
  ; dur : float
  }

let note ?(midi = 69) ?(dur = 0.2) phone = { phone; midi; dur }

let sing notes =
  let _, timed =
    List.fold notes ~init:(0., []) ~f:(fun (t, acc) { phone; midi; dur } ->
      let s = phone.render ~midi ~dur in
      let next_t = t +. Float.max 0.005 (dur -. phone.overlap) in
      next_t, (t, s) :: acc)
  in
  Signal.render (List.rev timed)
