open Base

let pi = 4.0 *. Float.atan 1.0
;;

let sine ?(amp=1.) freq =
  (fun t -> amp *. Float.sin (2. *. pi *. freq *. t))
  |> Signal.create_inf 

let saw freq =
  (fun t -> (Float.mod_float (t *. freq) 1.) *. 2. -. 1.)
  |> Signal.create_inf 

let square freq =
  (fun t -> (if Float.(mod_float (t *. freq) 1. < 0.5) then -1. else 1.))
  |> Signal.create_inf

let dc level = Signal.create_inf (Fn.const level)

let unpure_noise =
  (fun _t -> Random.float 2. -. 1.)
  |> Signal.create_inf 

let decay tau =
  (fun t -> Float.exp (-. t /. tau))
  |> Signal.create_inf

let ramp = Signal.create_inf Fn.id
