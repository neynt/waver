open Base
module Composition = Composition.Make ()

let song_demo _output_file =
  let open Composition in
  bpm := 240.
