open Base

(* Scales are indexed subsets of notes. Chords are small scales. *)

type t = { root : int; period : int; offsets : int array }
type degree = { index : int; accidental : [ `Sharp | `Flat | `Natural ] }

type roman_chord =
  [ `I | `II | `III | `IV | `V | `VI | `VII | `i | `ii | `iii | `iv | `v | `vi | `vii ]

let chromatic root =
  { root; period = 12; offsets = [| 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11 |] }

let major root = { root; period = 12; offsets = [| 0; 2; 4; 5; 7; 9; 11 |] }
let natural_minor root = { root; period = 12; offsets = [| 0; 2; 3; 5; 7; 8; 10 |] }
let minor = natural_minor
let maj_triad root = { root; period = 12; offsets = [| 0; 4; 7 |] }
let min_triad root = { root; period = 12; offsets = [| 0; 3; 7 |] }
let dim_triad root = { root; period = 12; offsets = [| 0; 3; 6 |] }
let aug_triad root = { root; period = 12; offsets = [| 0; 4; 8 |] }
let dream root = { root; period = 12; offsets = [| 0; 5; 6; 7 |] }
let maj_seventh root = { root; period = 12; offsets = [| 0; 4; 7; 11 |] }
let dom_seventh root = { root; period = 12; offsets = [| 0; 4; 7; 10 |] }
let min_seventh root = { root; period = 12; offsets = [| 0; 3; 7; 10 |] }
let min_maj_seventh root = { root; period = 12; offsets = [| 0; 3; 7; 11 |] }
let aug_maj_seventh root = { root; period = 12; offsets = [| 0; 4; 8; 11 |] }

let accidental_of_offset = function
  | -1 -> `Flat
  | 0 -> `Natural
  | 1 -> `Sharp
  | _ -> failwith "Accidentals don't go that far"

let offset_of_accidental = function
  | `Flat -> -1
  | `Natural -> 0
  | `Sharp -> 1

let from_accidental_count ?(quality = `Major) count =
  let major_tonic = (7 * count % 12) + 60 in
  match quality with
  | `Major -> major major_tonic
  | `Minor -> natural_minor (major_tonic - 3)

let scale_degree { root; period; offsets } midi =
  let leftover = midi - root in
  let octave = leftover /% period in
  let ofs = leftover - (octave * period) in
  let index =
    Option.value_exn
      (Array.binary_search
         offsets
         ~compare:Int.compare
         `First_greater_than_or_equal_to
         ofs)
  in
  let accidental = accidental_of_offset (ofs - offsets.(index)) in
  { index; accidental }

let at { root; period; offsets } i =
  let divmod a b = a /% b, a % b in
  let octave, ofs = divmod i (Array.length offsets) in
  root + (period * octave) + offsets.(ofs)

let subscale t indices =
  let root = at t (List.hd_exn indices) in
  let offsets = Array.of_list_map indices ~f:(fun i -> at t i - root) in
  { root; period = t.period; offsets }

let invert t ~steps =
  let root = at t steps in
  let offsets = Array.mapi t.offsets ~f:(fun i _ -> at t (steps + i) - root) in
  { root; period = t.period; offsets }

let shift_octaves t octaves = { t with root = t.root + (octaves * t.period) }
