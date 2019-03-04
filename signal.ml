open Base

type t =
  { f: float -> float
  ; dur: float
  }

let zero =
  { f = (fun _t -> 0.)
  ; dur = 0.
  }

let f t = t.f
let dur t = t.dur
let create f dur =
  { f = (fun t -> if Float.(<) t 0. then 0. else f t)
  ; dur
  }

let create_inf f = create f Float.infinity
let delay sec { f; dur } =
  { f = (fun t -> f (t -. sec))
  ; dur = dur +. sec
}

let crop sec { f; _ } =
  { f = (fun t -> if Float.(t <= sec) then f t else 0.); dur = sec }

let gain amp { f; dur } =
  { f = (fun t -> amp *. f t)
  ; dur
  }

let mul { f = f1; dur = dur1 } { f = f2; dur = dur2 } =
  { f = (fun t -> (f1 t) *. (f2 t))
  ; dur = Float.min dur1 dur2
  }

let add { f = f1; dur = dur1 } { f = f2; dur = dur2 } =
  { f = (fun t -> (f1 t) +. (f2 t))
  ; dur = Float.max dur1 dur2
  }

let memoize { f; dur } =
  let memo = Hashtbl.create (module Float) in
  { f = (fun t -> Hashtbl.find_or_add memo t ~default:(fun () -> f t))
  ; dur
  }

let render signals =
  let index = ref (Map.empty (module Float)) in
  let _ =
    List.sort signals ~compare:(fun (start1, _) (start2, _) -> Float.compare start1 start2)
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
      Map.closest_key !index `Less_or_equal_to t |> Option.value ~default:(Float.neg_infinity, [])
    in
    List.fold signals ~init:0. ~f:(fun acc signal -> acc +. signal.f t)
  in
  let dur =
    List.map signals ~f:(fun (start, { dur; _ }) -> start +. dur)
    |> List.fold ~init:0. ~f:Float.max
  in
  { f; dur }

(*
let render_slow signals =
  signals 
  |> List.map ~f:(fun (start, signal) -> delay start signal)
  |> List.reduce_balanced ~f:add
  |> Option.value ~default:zero
*)

let phase_mod { f = f_mod; _ } { f = f_orig; dur } =
  { f = (fun t -> f_orig (f_mod t))
  ; dur
  }

let freq freq { f; dur } =
  { f = (fun t -> f (t *. freq))
  ; dur
  }
;;

(* Linear frequency chirp. [f1] is the initial frequency multiplier; [f2] is
 * the frequency multiplier after [length] time. We obtained the constants by
 * by solving f'(t) = c1*t + c2; f'(0) = f1; f'(length) = f2; f(0) = 0
 *)
let chirp_lin f1 f2 length =
  let open Float in
  let c1 = 0.5 * ((f2 - f1) / length) in
  let c2 = f1 in
  phase_mod (create_inf (fun t -> c1*t*t + c2*t))
;;

(* Exponential frequency chirp. *)
let chirp_exp f1 f2 length =
  let open Float in
  let c1 = f1 in
  let c2 = log (f2 / f1) / length in
  let c3 = c1 / c2 in
  phase_mod (create_inf (fun t -> c3 * exp (c2 * t) - c3))
;;
