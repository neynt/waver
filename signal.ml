open Base

(* A [Signal.t] is a function over time annotated with a duration. This file
 * defines constructors, transformers, and combinators that operate on Signals.
 *
 * A lot could be simplified if we got rid of durations and simply operated on
 * functions over time. But that would be bad for performance.
 * *)
type t = { f : float -> float; dur : float } [@@deriving fields, sexp]

(* The zero signal is zero for zero time. *)
let zero = { f = (fun _t -> 0.); dur = 0. }
let create f dur = { f = (fun t -> if Float.( < ) t 0. then 0. else f t); dur }
let create_inf f = create f Float.infinity
let map { f = f'; dur } ~f = { f = Fn.compose f f'; dur }

let memoize { f; dur } =
  let memo = Hashtbl.create (module Float) in
  { f = (fun t -> Hashtbl.find_or_add memo t ~default:(fun () -> f t)); dur }

(* Built-in signals and signal constructors. *)
let pi = 4.0 *. Float.atan 1.0

let sine ?(amp = 1.) freq =
  (fun t -> amp *. Float.sin (2. *. pi *. freq *. t)) |> create_inf

let cosine ?(amp = 1.) freq =
  (fun t -> amp *. Float.cos (2. *. pi *. freq *. t)) |> create_inf

let saw freq = (fun t -> (Float.mod_float (t *. freq) 1. *. 2.) -. 1.) |> create_inf

let square ?(duty = 0.5) freq =
  (fun t -> if Float.(mod_float (t * freq) 1. < duty) then -1. else 1.) |> create_inf

let dc level = create_inf (Fn.const level)
let unpure_noise = (fun _t -> Random.float 2. -. 1.) |> create_inf
let noise () = memoize unpure_noise

(* Integrate the underlying signal, sampling [sr] times per second. *)
let integrate sr { f; dur } =
  let cache = Hashtbl.create (module Int) in
  Hashtbl.set cache ~key:0 ~data:(f 0.);
  let rec aux sample =
    Hashtbl.find_or_add cache sample ~default:(fun () ->
        aux (sample - 1) +. f (sample // sr))
  in
  { f = (fun t -> aux (Int.of_float (Float.round_nearest (t *. Float.of_int sr)))); dur }

let brown_noise () = noise () |> integrate 44100
let decay tau = (fun t -> Float.exp (-.t /. tau)) |> create_inf
let ramp = create_inf Fn.id

(* Signal operators *)
let delay sec { f; dur } = { f = (fun t -> f (t -. sec)); dur = dur +. sec }
let crop sec { f; _ } = { f = (fun t -> if Float.(t <= sec) then f t else 0.); dur = sec }
let gain amp { f; dur } = { f = (fun t -> amp *. f t); dur }

let mul { f = f1; dur = dur1 } { f = f2; dur = dur2 } =
  { f = (fun t -> f1 t *. f2 t); dur = Float.min dur1 dur2 }

let add { f = f1; dur = dur1 } { f = f2; dur = dur2 } =
  { f = (fun t -> f1 t +. f2 t); dur = Float.max dur1 dur2 }

let render signals =
  let index = ref (Map.empty (module Float)) in
  let (_ : (float * t) list) =
    List.sort signals ~compare:(fun (start1, _) (start2, _) ->
        Float.compare start1 start2)
    |> List.fold ~init:[] ~f:(fun acc (start, signal) ->
           let acc =
             List.filter acc ~f:(fun (start', _) -> Float.(start' > start))
             |> List.cons (start +. signal.dur, delay start signal)
           in
           index := Map.set !index ~key:start ~data:(List.map ~f:snd acc);
           acc)
  in
  let f t =
    let _, signals =
      Map.closest_key !index `Less_or_equal_to t
      |> Option.value ~default:(Float.neg_infinity, [])
    in
    List.fold signals ~init:0. ~f:(fun acc signal -> acc +. signal.f t)
  in
  let dur =
    List.map signals ~f:(fun (start, { dur; _ }) -> start +. dur)
    |> List.fold ~init:0. ~f:Float.max
  in
  { f; dur }

let render_slow signals =
  signals
  |> List.map ~f:(fun (start, signal) -> delay start signal)
  |> List.reduce_balanced ~f:add
  |> Option.value ~default:zero

let phase_mod { f = f_mod; _ } { f = f_orig; dur } =
  { f = (fun t -> f_orig (f_mod t)); dur }

let freq freq { f; dur } = { f = (fun t -> f (t *. freq)); dur }

(* Linear frequency chirp. [f1] is the initial frequency multiplier; [f2] is
 * the frequency multiplier after [length] time. We obtained the constants by
 * by solving f'(t) = c1*t + c2; f'(0) = f1; f'(length) = f2; f(0) = 0
 *)
let chirp_lin f1 f2 length =
  let open Float in
  let c1 = 0.5 * ((f2 - f1) / length) in
  let c2 = f1 in
  phase_mod (create_inf (fun t -> (c1 * t * t) + (c2 * t)))

(* Exponential frequency chirp. *)
let chirp_exp f1 f2 length =
  let open Float in
  let c1 = log (f2 / f1) / length in
  let c2 = f1 / c1 in
  phase_mod (create_inf (fun t -> (c2 * exp (c1 * t)) - c2))

let echo delay { f; dur } = { f = (fun t -> (f t +. f (t +. delay)) /. 2.); dur }

let pwlin ?(y0 = 0.0) transitions =
  let fns, _, _ =
    List.fold
      transitions
      ~init:(Map.empty (module Float), 0.0, y0)
      ~f:(fun (fns, t, y1) (dur, y2) ->
        (* f(t) = mt + b *)
        let t' = t +. dur in
        let m = (y2 -. y1) /. dur in
        let b = y1 -. (t *. m) in
        Map.add_exn fns ~key:t' ~data:(fun t -> (m *. t) +. b), t', y2)
  in
  { f =
      (fun t ->
        if Float.(t < 0.)
        then 0.
        else (Map.closest_key fns `Greater_or_equal_to t |> Option.value_exn |> snd) t)
  ; dur = Float.infinity
  }
