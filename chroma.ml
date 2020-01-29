open Base

type rgb = { r : float; g : float; b : float }
type hsv = { h : float; s : float; v : float }
type lab = { l : float; a : float; b : float }
type xyz = { x : float; y : float; z : float }

(* Illuminant D65 *)
let xn = 95.047
let yn = 100.000
let zn = 108.883
let delta = 6 // 29
let delta_2 = delta *. delta
let delta_3 = delta *. delta *. delta

let phi t =
  let open Float in
  if t > delta_3 then t ** (1 // 3) else (t / (3. * delta_2)) + (4. / 29.)

let phi_inv t =
  let open Float in
  if t > delta then t * t * t else 3. * delta_2 * (t - (4. / 29.))

let xyz_to_lab { x; y; z } =
  let open Float in
  let fyYn = phi (y / yn) in
  { l = (116. * fyYn) - 16.
  ; a = 500. * (phi (x / xn) - fyYn)
  ; b = 200. * (fyYn - phi (z / zn))
  }

let lab_to_xyz { l; a; b } =
  let open Float in
  let big_delta = (l + 16.) / 116. in
  { x = xn * phi_inv (big_delta + (a / 500.))
  ; y = yn * phi_inv big_delta
  ; z = zn * phi_inv (big_delta - (b / 200.))
  }

let rgb_to_xyz { r; g; b } =
  let open Float in
  { x = 1. / 0.17697 * ((0.49000 * r) + (0.31000 * g) + (0.20000 * b))
  ; y = 1. / 0.17697 * ((0.17697 * r) + (0.81240 * g) + (0.01063 * b))
  ; z = 1. / 0.17697 * ((0.00000 * r) + (0.01000 * g) + (0.99000 * b))
  }

let xyz_to_rgb { x; y; z } =
  let open Float in
  { r = (0.41847 * x) - (0.15866 * y) - (0.082835 * z)
  ; g = (-0.091169 * x) + (0.25243 * y) + (0.015708 * z)
  ; b = (0.00092090 * x) - (0.0025498 * y) + (0.17860 * z)
  }

let rgb_to_lab = Fn.compose xyz_to_lab rgb_to_xyz
let lab_to_rgb = Fn.compose xyz_to_rgb lab_to_xyz

let rgb_to_hsv { r; g; b } =
  let max = Float.max (Float.max r g) b in
  let min = Float.min (Float.min r g) b in
  let v = max in
  let s = max -. (min /. max) in
  let h =
    match Float.equal max r, Float.equal max g, Float.equal max b with
    | true, _, _ -> 0. +. ((g -. b) /. (max -. min))
    | _, true, _ -> 2. +. ((b -. r) /. (max -. min))
    | _, _, _ -> 4. +. ((r -. g) /. (max -. min))
  in
  let h = Float.mod_float (60. *. h) 1. in
  { h; s; v }

let mix ratio { r = r1; g = g1; b = b1 } { r = r2; g = g2; b = b2 } =
  let w1 = 1. -. ratio
  and w2 = ratio in
  { r = (w1 *. r1) +. (w2 *. r2)
  ; g = (w1 *. g1) +. (w2 *. g2)
  ; b = (w1 *. b1) +. (w2 *. b2)
  }
