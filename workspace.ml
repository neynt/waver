open Base

module Composition = Composition.Make ()

let song_demo output_file =
  let open Composition in
  bpm := 240.
