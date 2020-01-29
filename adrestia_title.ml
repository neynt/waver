open Base
module Compo = Composition.Make ()

let song_demo output_file =
  let open Compo in
  let open Instrument in
  bpm := 120.;
  let melody = melody ~instrument:(fun note dur -> choir (blip note dur)) in
  let chrom2 = Scale.chromatic 72 in
  let chrom = Scale.chromatic 48 in
  let scale = Scale.major 60 in
  let bass_scale = Scale.shift_octaves scale (-2) in
  melody chrom2 {| 3 - 2 - 3 - 0 - |};
  push_mark ();
  pop_mark ();
  melody chrom {| 15-- 17 19- 12--- ---- -- |};
  melody chrom {| 15-- 12 17--- ---- ---- |};
  melody chrom {| 19-- 21 23-- |};
  melody chrom {| 19---  |};
  together
    [ (fun () -> melody chrom {| 3 - 2 - 3 - 0 - |})
    ; (fun () -> melody chrom {| 10 - 10 - 10 - 10 - |})
    ];
  together
    [ (fun () -> melody chrom {| 3 - 2 - 3 - 0 - |})
    ; (fun () -> melody chrom {| 12 - 12 - 12 - 12 - |})
    ];
  together
    [ (fun () -> melody chrom {| 3 - 2 - 3 - 0 - |})
    ; (fun () -> melody chrom {| 12 - 12 - 12 - 12 - |})
    ];
  start_here ();
  together
    [ (fun () ->
        play_perc
          {|
      . s . s . s . s
      . s . s . s . s
      . s . s . s . s
      . s . s . s . s
    |}
          [ 'k', kick; 's', hihat ])
    ; (fun () ->
        melody
          bass_scale
          {|
      3 7 5 7 3 7 5 7
      4 6 8 6 4 6 8 6
      2 6 4 6 2 6 4 6
      5 7 9 7 5 7 9 7
    |})
    ];
  together
    [ (fun () ->
        play_perc
          {|
      k s . s k s . s
      k s . s k s . s
      k s . s k s . s
      k s . s k s . s
    |}
          [ 'k', kick; 's', hihat ])
    ; (fun () ->
        melody
          bass_scale
          {|
      3 7 5 7 3 7 5 7
      4 6 8 6 4 6 8 6
      2 6 4 6 2 6 4 6
      5 7 9 7 5 7 9 7
    |})
    ; (fun () ->
        melody
          scale
          {|
      0 - 3 - 4 - 5 -
      6 - - - 7 6 5 6
      9 - - - 8 7 6 5
      2 - 3 - 4 - - -
    |})
    ];
  together
    [ (fun () ->
        play_perc
          {|
      k h . h k h . h
      k h . h k h . h
      k h . h k h . h
      k h . h k h . h
    |}
          [ 'k', kick; 'h', hihat ])
    ; (fun () ->
        melody
          bass_scale
          {|
      0 4 2 4 0 4 2 4
      5 9 7 9 5 9 7 9
      3 5 7 5 3 5 7 5
      4 6 8 6 4 6 8 6
    |})
    ; (fun () ->
        melody
          scale
          {|
      0 - - - 1 - - -
      2 - - - 3 - - -
      5 - - - 6 - - -
      4 - - - - - - -
    |})
    ];
  end_here ();
  let song = Signal.render (result ()) in
  Wav.save ~sampling_rate:44100 output_file [ song ]
