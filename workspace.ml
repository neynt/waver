open Base
module Compo = Composition.Make ()

let render output_file =
  let open Compo in
  let open Instrument in
  bpm := 130.;
  let play beats note = play beats (blaap note (bt beats)) in
  let melody = melody ~instrument:(fun note dur -> blip note dur) in
  let chrom2 = Scale.chromatic 72 in
  let chrom = Scale.chromatic 48 in
  let scale = Scale.major 48 in
  let bass_scale = Scale.shift_octaves scale (-2) in
  start_here ();
  let harmony ?(rot = 0) scale =
    List.init 9 ~f:(fun i () -> play 2 (Scale.at scale (i + rot))) |> together
  in
  let _I = Scale.(maj_triad (at scale 0)) in
  let _ii = Scale.(min_triad (at scale 1)) in
  let _iii = Scale.(min_triad (at scale 2)) in
  let _IV = Scale.(maj_triad (at scale 3)) in
  let _V = Scale.(maj_triad (at scale 4)) in
  let _vi = Scale.(min_triad (at scale 5)) in
  let _vii = Scale.(dim_triad (at scale 6)) in
  (*
  harmony _ii;
  harmony ~rot:(-1) _iii;
  harmony ~rot:(-1) _vi;
  harmony ~rot:(-1) _ii;
  harmony _ii;
  harmony ~rot:(-1) _iii;
  harmony _V;
  harmony ~rot:(-1) _I;
*)
  harmony _vi;
  harmony _IV;
  harmony _V;
  harmony _I;
  harmony _vi;
  harmony _IV;
  harmony _V;
  harmony _vii;
  end_here ();
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
  let song = Signal.render (result ()) in
  Wav.save ~sampling_rate:44100 output_file [ song ]
