open Base

type t =
  { f: float -> float
  ; dur: float
  }
;;

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
let at { f; dur } t = if Float.(<) t 0. || Float.(>) t dur then 0. else f t
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

let render (signals : (float * t) list) =
  let index = ref (Map.empty (module Float)) in
  let _ =
    List.sort signals ~compare:(fun (t1, _) (t2, _) -> Float.compare t1 t2)
    |> List.fold ~init:[] ~f:(fun acc (t, signal) -> 
        let acc = List.filter acc ~f:(fun (t', _) -> Float.(t' > t)) in
        let acc = List.cons (t +. signal.dur, delay t signal) acc in
        index := Map.set !index ~key:t ~data:(List.map ~f:snd acc);
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

let _render_slow signals =
  signals 
  |> List.map ~f:(fun (start, signal) -> delay start signal)
  |> List.reduce_balanced ~f:add
  |> Option.value ~default:zero
