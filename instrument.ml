open Base
open Signal
open Shape

let blip ?(velo = 1.0) midi len =
  let freq = Temperament.equal midi in
  saw freq
  |> phase_mod (add ramp (sine 8. |> add (dc 1.) |> gain (0.2 /. freq)))
  |> mul (decay (Float.max (len /. 3.0) 0.2))
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

let snare =
  unpure_noise
  |> crop 0.5
  |> gain 0.05
  |> mul (decay 0.05)
;;
