(* Conversions between [Signal.t] and [Discrete_signal.t]. *)
open Base

let zero_order_hold ds =
  let { Discrete_signal.samples; sample_rate } = ds in
  let samples = Array.copy samples in
  let length = Array.length samples in
  let dur = length // sample_rate in
  let sample_rate = Int.to_float sample_rate in
  let f t =
    let i = Int.of_float_unchecked (Float.round_nearest (t *. sample_rate)) in
    if i >= 0 && i < length then samples.(i) else 0.
  in
  { Signal.f; dur }

let discretize s ~sample_rate =
  let { Signal.f; dur } = s in
  let length = Float.iround_nearest_exn (dur *. Int.to_float sample_rate) in
  let samples = Array.init length ~f:(fun i -> f (i // sample_rate)) in
  { Discrete_signal.samples; sample_rate }
