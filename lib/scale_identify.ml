(* Tools to identify the quality of a chord (e.g. major triad) or scale (e.g.
 * diatonic). *)

type chord_quality = { name : string; characteristic : int list; offset : int }

let chords : chord_quality list =
  [ { name = "major triad"; characteristic = [ 3; 5; 4 ]; offset = -1 }
  ; { name = "minor triad"; characteristic = [ 3; 4; 5 ]; offset = 0 }
  ]

let lexicographically_minimum_rotation _xs = failwith "unimplemented"
