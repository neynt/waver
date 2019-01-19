open Base

let pi = 4.0 *. Float.atan 1.0
;;

let sine freq =
  (fun t -> t *. 2. *. pi *. freq)
  |> Signal.create_inf 

let saw freq =
  (fun t -> (Float.mod_float (t *. freq) 1.) *. 2. -. 1.)
  |> Signal.create_inf 

let unpure_noise =
  (fun _t -> Random.float 2. -. 1.)
  |> Signal.create_inf 

let decay tau =
  (fun t -> Float.exp (-. t /. tau))
  |> Signal.create_inf
