(** A library of basic instruments for exploration. *)

open Base
open Signal

(* [f1]: first frequency factor
 * [f2]: second frequency factor
 * [period]: vibrato frequency
 *)
let vibrato f1 f2 period =
  let mean = (f1 +. f2) /. 2. in
  let amp = Float.abs ((f1 -. f2) /. 2.) in
  phase_mod
    (add (ramp |> gain mean) (cosine period |> gain (amp /. (2. *. pi *. period))))

let bloop_base ~wave ?(velo = 1.0) midi len =
  let freq = Temperament.equal 440. midi in
  wave freq
  |> vibrato 1. 1.03 13.
  |> mul (decay (Float.max (len /. 3.0) 0.07))
  |> gain (0.05 *. velo)
  |> crop len

let choir f = f |> add (freq 1.01 f) |> add (freq 0.997 f) |> gain (1 // 3)
let blip = bloop_base ~wave:(square ~duty:0.5)
let bleep = bloop_base ~wave:saw
let bloop = bloop_base ~wave:sine
let kick = square 200. |> chirp_exp 1. 0.4 0.1 |> crop 0.1 |> gain 0.05 |> mul (decay 0.1)
let hihat = unpure_noise |> crop 0.05 |> gain 0.05 |> mul (decay 0.05)
