type t =
  { f: float -> float
  ; dur: float
  }
;;

val zero : t
val f : t -> float -> float
val dur : t -> float
val create_inf : (float -> float) -> t
val create : (float -> float) -> float -> t
val delay : float -> t -> t
val crop : float -> t -> t
val gain : float -> t -> t
val mul : t -> t -> t
val add : t -> t -> t
val memoize : t -> t
val render : (float * t) list -> t
val phase_mod : t -> t -> t
val freq : float -> t -> t
val chirp_lin : float -> float -> float -> t -> t
val chirp_exp : float -> float -> float -> t -> t
