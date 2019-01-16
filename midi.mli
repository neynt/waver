type note =
  { time: int
  ; midi: int
  ; chan: int
  ; velo: int
  }

type t =
  { time_unit: float
  ; tracks: note list list
  }

val do_stuff : unit -> t
