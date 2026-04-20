(** Chiptune x vocaloid x breakcore instruments and effects. *)
open Base

open Signal

let tau = 2. *. pi

(* Wrap a signal in slow vibrato. [depth] is roughly fractional pitch deviation. *)
let with_vibrato ~depth ~rate s =
  let modulator = add ramp (cosine rate |> gain (depth /. (tau *. rate))) in
  phase_mod modulator s

(* PxTone-ish organ: narrow square + sub-octave + airy detuned harmonic, gentle
   vibrato, trapezoidal envelope. Works for held chord tones. *)
let pxtone_organ ?(velo = 1.0) midi dur =
  let freq = Temperament.equal 440. midi in
  let body =
    add (square ~duty:0.25 freq) (square ~duty:0.5 (freq /. 2.) |> gain 0.35)
    |> add (sine (freq *. 2.005) |> gain 0.15)
  in
  body
  |> with_vibrato ~depth:0.004 ~rate:5.5
  |> mul (pwlin [ 0.04, 1.; Float.max 0.02 (dur -. 0.09), 1.; 0.05, 0. ])
  |> gain (0.045 *. velo)
  |> crop dur

(* Chiptune pulse lead: PWM-sweeping square with fast attack and light vibrato. *)
let pulse_lead ?(velo = 1.0) midi dur =
  let freq = Temperament.equal 440. midi in
  let duty_env = pwlin ~y0:0.5 [ Float.max 0.01 dur, 0.22 ] in
  let wave =
    create_inf (fun t ->
      let d = duty_env.f t in
      let p = Float.mod_float (t *. freq) 1. in
      if Float.(p < d) then -1. else 1.)
  in
  wave
  |> with_vibrato ~depth:0.005 ~rate:6.5
  |> mul (pwlin [ 0.004, 1.; 0.02, 0.85; Float.max 0.01 (dur -. 0.06), 0.85; 0.035, 0. ])
  |> gain (0.1 *. velo)
  |> crop dur

(* Vowel synth: additive harmonics on a 1/n source, weighted by lorentzian
   formant peaks (sharper & more defined than a soft exponential falloff).
   [formants] is a list of [(center_hz, bandwidth_hz, peak_amp)] triples. *)
let vowel
      ?(n_harmonics = 32)
      ?(velo = 1.0)
      ?(breath = 0.02)
      ?(vib_depth = 0.006)
      ?(vib_rate = 5.0)
      ~formants
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
      sine fn |> gain w)
    |> List.reduce_balanced ~f:add
    |> Option.value ~default:zero
  in
  let body =
    if Float.(breath <= 0.) then harmonics else add harmonics (unpure_noise |> gain breath)
  in
  body
  |> with_vibrato ~depth:vib_depth ~rate:vib_rate
  |> mul (pwlin [ 0.08, 1.; Float.max 0.02 (dur -. 0.22), 1.; 0.14, 0. ])
  |> gain (0.022 *. velo)
  |> crop dur

(* Vowel formant presets: [(center_hz, bandwidth_hz, amp)]. Tighter bandwidths
   give sharper, more vowel-like peaks. *)
let ah_formants =
  [ 730., 90., 1.0; 1090., 110., 0.85; 2440., 130., 0.55; 3400., 160., 0.25 ]

let ee_formants = [ 270., 70., 1.0; 2290., 110., 1.0; 3010., 130., 0.6; 3800., 160., 0.2 ]
let oo_formants = [ 300., 80., 1.0; 870., 90., 0.7; 2240., 120., 0.3; 3200., 140., 0.1 ]
let oh_formants = [ 570., 80., 1.0; 840., 90., 0.8; 2410., 120., 0.4; 3300., 140., 0.1 ]
let eh_formants = [ 530., 80., 1.0; 1840., 110., 0.8; 2480., 130., 0.5; 3500., 160., 0.2 ]
let uh_formants = [ 640., 80., 1.0; 1190., 100., 0.6; 2390., 130., 0.3; 3200., 140., 0.1 ]
let ah ?velo ?breath midi dur = vowel ~formants:ah_formants ?velo ?breath midi dur
let ee ?velo ?breath midi dur = vowel ~formants:ee_formants ?velo ?breath midi dur
let oo ?velo ?breath midi dur = vowel ~formants:oo_formants ?velo ?breath midi dur
let oh ?velo ?breath midi dur = vowel ~formants:oh_formants ?velo ?breath midi dur
let eh ?velo ?breath midi dur = vowel ~formants:eh_formants ?velo ?breath midi dur
let uh ?velo ?breath midi dur = vowel ~formants:uh_formants ?velo ?breath midi dur

(* Backward-compat alias — the old [vocaloid_ah] now uses the sharper synth. *)
let vocaloid_ah = ah

(* Sub bass: triangle with a splash of 2nd harmonic; tight decay. *)
let sub_bass ?(velo = 1.0) midi dur =
  let freq = Temperament.equal 440. midi in
  add (triangle freq) (sine (freq *. 2.) |> gain 0.15)
  |> mul (pwlin [ 0.005, 1.; Float.max 0.01 (dur -. 0.04), 1.; 0.035, 0. ])
  |> gain (0.18 *. velo)
  |> crop dur

(* Punchy snare: pitched thump + filtered noise. *)
let snare =
  let tone = triangle 220. |> chirp_exp 1. 0.3 0.04 |> mul (decay 0.06) |> gain 0.45 in
  let body = unpure_noise |> mul (decay 0.08) |> gain 0.5 in
  add tone body |> crop 0.15 |> gain 0.25

(* Crisp closed hihat — short noise ping. *)
let hat_closed = unpure_noise |> crop 0.03 |> mul (decay 0.02) |> gain 0.07

(* Open hihat — longer decay. *)
let hat_open = unpure_noise |> crop 0.15 |> mul (decay 0.09) |> gain 0.06

(* Stacked noise bursts for a clap feel. *)
let clap =
  let burst = unpure_noise |> crop 0.02 |> mul (decay 0.015) in
  render [ 0.0, burst; 0.008, burst; 0.018, burst ] |> gain 0.25 |> crop 0.12

(* Long crash cymbal. *)
let crash = unpure_noise |> mul (decay 0.4) |> gain 0.14 |> crop 0.9

(* [stutter n s]: take the first [s.dur / n] of [s] and repeat [n] times to fill. *)
let stutter n s =
  let each = s.dur /. Float.of_int n in
  let piece = crop each s in
  List.init n ~f:(fun i -> Float.of_int i *. each, piece) |> render

(* Soft tanh saturation — [drive] around 2-5 is a sweet spot. *)
let distort drive s =
  let k = Float.tanh drive in
  map s ~f:(fun x -> Float.tanh (x *. drive) /. k)

(* Amplitude quantization to 2^bits levels. *)
let bitcrush bits s =
  let levels = Float.of_int (1 lsl bits) in
  map s ~f:(fun x ->
    let c = Float.max (-1.) (Float.min 1. x) in
    Float.round_nearest (c *. levels) /. levels)

(* Plucky 8-bit harpsichord — stacked sines with a tight exponential decay. *)
let pluck ?(velo = 1.0) midi dur =
  let freq = Temperament.equal 440. midi in
  let decay_tau = Float.max 0.05 (dur *. 0.6) in
  add (sine freq) (sine (freq *. 2.) |> gain 0.35)
  |> add (sine (freq *. 3.) |> gain 0.15)
  |> mul (decay decay_tau)
  |> mul (pwlin [ 0.003, 1.; Float.max 0.01 (dur -. 0.02), 1.; 0.015, 0. ])
  |> gain (0.1 *. velo)
  |> crop dur

(* Bell / chime — inharmonic partials, long decay. *)
let bell ?(velo = 1.0) midi dur =
  let freq = Temperament.equal 440. midi in
  let partial ratio g tau = sine (freq *. ratio) |> gain g |> mul (decay tau) in
  partial 1.00 1.00 0.80
  |> add (partial 2.76 0.55 0.55)
  |> add (partial 5.40 0.28 0.35)
  |> add (partial 8.93 0.14 0.22)
  |> mul (pwlin [ 0.003, 1.; Float.max 0.01 (dur -. 0.01), 1.; 0.005, 0. ])
  |> gain (0.08 *. velo)
  |> crop dur

(* Portamento pulse lead — pitch slides linearly from [midi_from] to [midi_to]
   over [dur]. Phase is integrated analytically so the slide stays in tune. *)
let portamento_lead ?(velo = 1.0) midi_from midi_to dur =
  let f1 = Temperament.equal 440. midi_from in
  let f2 = Temperament.equal 440. midi_to in
  let phase =
    create_inf (fun t ->
      let t = Float.min t dur in
      (f1 *. t) +. ((f2 -. f1) *. t *. t /. (2. *. dur)))
  in
  let unit_pulse =
    create_inf (fun p -> if Float.(Float.mod_float p 1. < 0.3) then -1. else 1.)
  in
  phase_mod phase unit_pulse
  |> with_vibrato ~depth:0.004 ~rate:6.
  |> mul (pwlin [ 0.005, 1.; 0.02, 0.85; Float.max 0.01 (dur -. 0.055), 0.85; 0.03, 0. ])
  |> gain (0.1 *. velo)
  |> crop dur

(* Growl bass — pitched square with exponential frequency drop for an attack-y
   low-tom feel. *)
let growl_bass ?(velo = 1.0) midi dur =
  let freq = Temperament.equal 440. midi in
  square ~duty:0.25 freq
  |> chirp_exp 1.2 0.5 (Float.max 0.05 (dur /. 2.))
  |> mul (decay (Float.max 0.08 (dur *. 0.5)))
  |> mul (pwlin [ 0.005, 1.; Float.max 0.01 (dur -. 0.04), 1.; 0.03, 0. ])
  |> gain (0.16 *. velo)
  |> crop dur

(* Rim-shot click — pitched tick + air burst. *)
let rimshot =
  let click = triangle 1800. |> mul (decay 0.005) |> gain 0.4 in
  let air = unpure_noise |> mul (decay 0.02) |> gain 0.2 in
  add click air |> crop 0.04 |> gain 0.25
