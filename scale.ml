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

let at { tonic; period; offsets } i =
  let divmod a b = a / b, a % b in
  let octave, ofs = divmod i (Array.length offsets) in
  tonic + (period * octave) + Array.get offsets ofs

(*
let range { tonic; period; offsets } lower upper =
  failwith "unimplemented"
;;
*)
