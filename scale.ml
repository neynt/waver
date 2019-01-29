open Base

type t =
  { tonic: int
  ; period: int
  ; offsets: int array
  }

type degree =
  { index: int
  ; accidental: [`Sharp | `Flat | `Natural]
  }

let major tonic =
  { tonic
  ; period = 12
  ; offsets = [|0; 2; 4; 5; 7; 9; 11|]
  }

let natural_minor tonic =
  { tonic
  ; period = 12
  ; offsets = [|0; 2; 3; 5; 7; 8; 10|]
  }

let accidental_of_offset = function
  | -1 -> `Flat
  | 0 -> `Natural
  | 1 -> `Sharp
  | _ -> failwith "Accidentals don't go that far"
;;

let offset_of_accidental = function
  | `Flat -> -1
  | `Natural -> 0
  | `Sharp -> 1
;;

let from_accidental_count ?(quality = `Major) count =
  let major_tonic = (7 * count) % 12 + 60 in
  match quality with
  | `Major -> major major_tonic
  | `Minor -> natural_minor (major_tonic - 3)
;;

let scale_degree { tonic; period; offsets } midi =
  let leftover = midi - tonic in
  let octave = leftover /% period in
  let ofs = leftover - octave * period in
  let index =
    Option.value_exn
      (Array.binary_search offsets ~compare:Int.compare `First_greater_than_or_equal_to ofs)
  in
  let accidental = accidental_of_offset (ofs - (Array.get offsets index)) in
  { index; accidental }
;;

let at { tonic; period; offsets } i =
  let divmod a b = a / b, a % b in
  let octave, ofs = divmod i (Array.length offsets) in
  tonic + (period * octave) + Array.get offsets ofs
;;
