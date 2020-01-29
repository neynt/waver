open Base

type t = int -> float

let equal a4_pitch midi = a4_pitch *. (2. **. (Float.of_int (midi - 69) /. 12.))
let equal_inv a4_pitch freq = (Float.log (freq /. a4_pitch) /. Float.log 2. *. 12.) +. 69.

let pythagorean =
  let _factors =
    [| 1.
     ; 256 // 243
     ; 9 // 8
     ; 32 // 27
     ; 81 // 64
     ; 4 // 3
     ; 729 // 512
     ; 1024 // 729
     ; 3 // 2
     ; 128 // 81
     ; 27 // 16
     ; 16 // 9
     ; 243 // 128
    |]
  in
  fun (_tonic : int) (_midi : int) -> failwith "unimplemented"
