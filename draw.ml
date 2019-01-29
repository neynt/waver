open Base

let make_frame () = new OImages.rgb24 1920 1080

let rgb r g b = { Color. r; g; b }

let rect (image: OImages.rgb24) x y w h c =
  for x = (Int.max 0 x) to Int.min (x + w - 1) (image#width - 1) do
    for y = (Int.max 0 y) to Int.min (y + h - 1) (image#height - 1) do
      image#set x y c
    done
  done

let striped_rect (image: OImages.rgb24) x y w h stripe_width c1 c2 =
  for x = (Int.max 0 x) to Int.min (x + w - 1) (image#width - 1) do
    for y = (Int.max 0 y) to Int.min (y + h - 1) (image#height - 1) do
      let color = match (x + y) / stripe_width % 2 with
      | 0 -> c1
      | _ -> c2
      in
      image#set x y color
    done
  done

let fill (image: OImages.rgb24) c =
  rect image 0 0 image#width image#height c

let split start stop intervals =
  Array.map intervals ~f:(fun i ->
    start
    + (Float.of_int (stop - start) *. i
    |> Float.round_nearest
    |> Int.of_float))

let split_equal start stop num =
  Array.init (num + 1) ~f:Fn.id
  |> Array.map ~f:(fun i -> i // num)
  |> split start stop

let piano
  ?(white_color = rgb 128 128 128)
  ?(black_color = rgb 0 0 0)
  ?(semitone_width = 21)
  (image: OImages.rgb24)
  min_note
  max_note
=
  let spacing = 1
  and black_height = 100
  and white_height = 150
  in
  let octave_width = semitone_width * 12 in
  let semitone_places = split_equal 0 octave_width 12 in
  let breakpoint = Array.get semitone_places 5 in
  let white_3_places = split_equal 0 breakpoint 3 in
  let white_4_places = split_equal breakpoint octave_width 4 in
  let xofs =
    let octave = min_note / 12 in
    let idx = min_note % 12 in
    - (octave * octave_width + Array.get semitone_places idx)
  in
  let top_start = (fun midi ->
    let octave = midi / 12 in
    let idx = midi % 12 in
    xofs + octave * octave_width + Array.get semitone_places idx)
  in
  let bot_start = (fun white_idx ->
    let idx = white_idx % 7 in
    (white_idx / 7) * octave_width
    + match idx with
    | 0 | 1 | 2 -> Array.get white_3_places idx
    | _ -> Array.get white_4_places (idx - 3))
  in
  let yofs = image#height - white_height in
  for midi = min_note to max_note do 
    let octave = midi / 12 in
    let idx = midi % 12 in
    match idx with
    | 1 | 3 | 6 | 8 | 10 ->
      let x = top_start midi + spacing in
      let y = yofs + spacing in
      let w = semitone_width - 2 * spacing in
      let h = black_height - spacing in
      rect image x y w h black_color;
    | _ ->
      let x = top_start midi + spacing in
      let y = yofs + spacing in
      let w = semitone_width - 2 * spacing in
      let h = white_height - spacing in
      rect image x y w h white_color;
      let white_idx = ((midi % 12) + 1) / 2 in
      let x = xofs + bot_start white_idx + spacing in
      let y = yofs + black_height + 2 * spacing in
      let w = (bot_start (white_idx + 1)) - (bot_start white_idx) - 2 * spacing in
      let x = x + octave_width * octave in
      let h = (white_height - black_height - 2 * spacing) in
      rect image x y w h white_color;
  done;
  top_start
