open Base

type note =
  { time: float
  ; dur: float
  ; midi: int
  ; chan: int
  ; velo: int
  }

type key_signature =
  { accidentals: int
  ; quality: [ `Major | `Minor ]
  }

type t =
  { tracks: note list list
  ; key_signatures: (float, key_signature, Float.comparator_witness) Map.t
  }

val read_file : string -> t
