(** archive of unused code

  let _I = Scale.(maj_triad (at scale 0)) in
  let _ii = Scale.(min_triad (at scale 1)) in
  let _iii = Scale.(min_triad (at scale 2)) in
  let _IV = Scale.(maj_triad (at scale 3)) in
  let _V = Scale.(maj_triad (at scale 4)) in
  let _vi = Scale.(min_triad (at scale 5)) in
  let _vii = Scale.(dim_triad (at scale 6)) in
  let h = harmony ~len:4 in
  let rot = -3 in
  together [ (fun () -> h _IV ~rot); (fun () -> melody scale {| 1 - 5 - |}) ];
  together [ (fun () -> h _V ~rot); (fun () -> melody scale {| 2 4 - 5 |}) ];
  together [ (fun () -> h _vi ~rot); (fun () -> melody scale {| 3 - 5 - |}) ];
  together [ (fun () -> h _V ~rot); (fun () -> melody scale {| 4 5 - - |}) ];
  harmony _vi;
  harmony _IV;
  harmony _V;
  harmony _I;
  harmony _vi;
  harmony _IV;
  harmony _V;
  harmony _vii;
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
    [ (fun () -> melody chrom {|  3 -  2 -  3 -  0 - |})
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

 *)
