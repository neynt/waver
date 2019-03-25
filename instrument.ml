open Base
open Signal

(* [f1]: first frequency factor
 * [f2]: second frequency factor
 * [period]: vibrato frequency
*)
let vibrato f1 f2 period =
  let mean = (f1 +. f2) /. 2. in
  let amp = Float.abs ((f1 -. f2) /. 2.) in
  phase_mod (add (ramp |> gain mean) (cosine period |> (gain (amp /. (2. *. pi *. period)))))
;;

let blip ?(velo = 1.0) midi len =
  let freq = Temperament.equal midi in
  square ~duty:0.8 freq
  |> vibrato 1. 1.03 13.
  |> mul (decay (Float.max (len /. 3.0) 0.07))
  |> gain (0.05 *. velo)
  |> crop len
;;

let kick =
  square 200.
  |> chirp_exp 1. 0.4 0.1
  |> crop 0.1
  |> gain 0.05
  |> mul (decay 0.1)
;;

let hihat =
  unpure_noise
  |> crop 0.05
  |> gain 0.05
  |> mul (decay 0.05)
;;
