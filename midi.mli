type note =
  { time: float
  ; dur: float
  ; midi: int
  ; chan: int
  ; velo: int
  }

type t =
  { tracks: note list list
  }

val read_file : string -> t
