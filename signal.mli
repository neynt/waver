type t =
  { f: float -> float
  ; dur: float
  }
[@@deriving sexp]

val zero : t
val f : t -> float -> float
val dur : t -> float
val create_inf : (float -> float) -> t
val create : (float -> float) -> float -> t

val pi : float
val sine : ?amp:float -> float -> t
val cosine : ?amp:float -> float -> t
val saw : float -> t
val square : ?duty:float -> float -> t
val dc : float -> t
val unpure_noise : t
val noise : unit -> t
val brown_noise : unit -> t
val decay : float -> t
val ramp : t

val delay : float -> t -> t
val crop : float -> t -> t
val gain : float -> t -> t
val mul : t -> t -> t
val add : t -> t -> t
val memoize : t -> t
val render : (float * t) list -> t
val render_slow : (float * t) list -> t
val phase_mod : t -> t -> t
val freq : float -> t -> t
val chirp_lin : float -> float -> float -> t -> t
val chirp_exp : float -> float -> float -> t -> t
val echo : float -> t -> t
val pwlin : ?y0:float -> (float * float) list -> t
