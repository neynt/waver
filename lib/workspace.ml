open Base
module Compo = Composition.Make ()

let render output_file =
  let open Compo in
  let open Instrument in
  bpm := 120.;
  let play beats note = play beats (blaap note (bt beats)) in
  start_here ();
  together [ (fun () -> play 2 72); (fun () -> play 2 76) ];
  together [ (fun () -> play 2 70); (fun () -> play 2 77) ];
  end_here ();
  let song = Signal.render (result ()) in
  Wav.save ~sampling_rate:44100 output_file [ song ]
