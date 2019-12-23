open Base

let frame_demo input_file =
  let deg_to_color deg (quality : [ `Major | `Minor ]) =
    let deg = match quality with `Major -> deg | `Minor -> deg + 3 in
    let open Draw in
    match deg with
    | 0 -> rgb 255 0 0
    | 1 -> rgb 255 128 0
    | 2 -> rgb 255 255 0
    | 3 -> rgb 0 255 0
    | 4 -> rgb 0 140 255
    | 5 -> rgb 0 64 255
    | _ -> rgb 220 0 255
  in
  let midi_file = Midi.read_file input_file in
  let track =
    List.concat midi_file.tracks
    |> List.sort ~compare:(fun { Midi.time = time1; _ } { time = time2; _ } ->
           Float.compare time1 time2)
  in
  let min_note, max_note =
    List.fold track ~init:(Int.max_value, Int.min_value)
      ~f:(fun (min, max) { midi; _ } -> (Int.min min midi, Int.max max midi))
  in
  let min_note = Int.round_down ~to_multiple_of:12 (min_note - 1) in
  let max_note = Int.round_up ~to_multiple_of:12 (max_note + 1) - 1 in
  let max_time =
    List.fold track ~init:0. ~f:(fun max { time; dur; _ } ->
        Float.max max (time +. dur))
  in
  let t_to_y t =
    1080 - 150
    - Int.of_float
        (Float.min
           (Float.of_int Int.max_value_30_bits)
           (Float.round (t *. 120.)))
  in

  let chr_to_col { Chroma.r; g; b } =
    let f x = Int.of_float (Float.round_nearest (x *. 255.)) in
    { Color.r = f r; g = f g; b = f b }
  in
  let col_to_chr { Color.r; g; b } =
    let f x = x // 255 in
    { Chroma.r = f r; g = f g; b = f b }
  in
  let white_color = { Chroma.r = 0.5; g = 0.5; b = 0.5 } in
  let black_color = { Chroma.r = 0.; g = 0.; b = 0. } in

  let save_frame time_ofs filename =
    let frame = Draw.make_frame () in
    Draw.(fill frame (rgb 16 16 16));
    let semitone_width = 15 in
    let midi_to_x = Draw.piano ~semitone_width frame min_note max_note in
    Map.iteri midi_file.key_signatures ~f:(fun ~key ~data ->
        let time = key -. time_ofs in
        let key_signature = data in
        let time' =
          ( Map.closest_key midi_file.key_signatures `Greater_than key
          |> Option.value_map ~default:Float.infinity ~f:fst )
          -. time_ofs
        in
        (* Caml.Printf.printf "From %f to %f: Key signature %d\n" time time' data.accidentals; *)
        let scale =
          Scale.from_accidental_count ~quality:key_signature.quality
            key_signature.accidentals
        in
        for midi = min_note to max_note do
          let x = midi_to_x midi in
          let start_y = Int.min (1080 - 150) (t_to_y time) in
          let end_y = t_to_y time' in
          let h = start_y - end_y in
          let deg = Scale.scale_degree scale midi in
          match deg.accidental with
          | `Natural -> ()
          | _ ->
              Draw.(
                striped_rect frame (x + 1) end_y (semitone_width - 2) h 5
                  (rgb 48 48 48) (rgb 16 16 16))
        done);
    for midi = min_note to max_note + 1 do
      let x = midi_to_x midi in
      Draw.(rect frame (x - 1) 0 2 (1080 - 150) (rgb 48 48 48))
    done;
    let color_overrides = ref (Map.empty (module Int)) in
    List.iter track ~f:(fun { midi; time; dur; _ } ->
        let time' = time -. time_ofs in
        let { Midi.quality; accidentals } =
          Midi.key_signature_at midi_file time
        in
        let x = midi_to_x midi in
        let x' = midi_to_x (midi + 1) in
        let start_y = Int.min (1080 - 150) (t_to_y time') in
        let end_y = t_to_y (time' +. dur) in
        let h = start_y - end_y in
        let scale = Scale.from_accidental_count ~quality accidentals in
        let deg = Scale.scale_degree scale midi in
        let color = deg_to_color deg.index quality in
        let style : Draw.rect_style =
          let deg' =
            match deg.accidental with
            | `Natural -> None
            | `Flat -> Some (deg.index - 1)
            | `Sharp -> Some (deg.index + 1)
          in
          match deg' with
          | None -> Flat color
          | Some deg' ->
              let color' =
                deg_to_color (deg' % Array.length scale.offsets) quality
              in
              Striped (5, color, color')
        in
        Draw.rect_gen frame (x + 1) end_y (x' - x - 2) h style;
        Draw.(
          rect frame (x + 1) (start_y - 1)
            (x' - x - 2)
            (Int.min 1 h) (rgb 16 16 16));
        let fade_time = 0.1 in
        match Float.(time' <= 0. && time' +. dur >= 0.) with
        | true ->
            let fadedness = Float.min 1. ((time' +. dur) /. fade_time) in
            let key_color =
              match Draw.key_color midi with
              | `Black -> black_color
              | `White -> white_color
            in
            let style : Draw.rect_style =
              let mix c =
                chr_to_col (Chroma.mix fadedness key_color (col_to_chr c))
              in
              match style with
              | Flat color -> Flat (mix color)
              | Striped (w, color, color') -> Striped (w, mix color, mix color')
            in
            color_overrides := Map.set !color_overrides ~key:midi ~data:style
        | _ -> ());
    let (_ : int -> int) =
      Draw.piano ~semitone_width ~white_color:(chr_to_col white_color)
        ~black_color:(chr_to_col black_color) ~color_overrides:!color_overrides
        frame min_note max_note
    in
    frame#save filename None []
  in
  (max_time, save_frame)

let video save_frame max_time output_dir fps =
  let max_frame =
    Int.of_float (Float.round_up (Float.of_int fps *. max_time))
  in
  Caml.Printf.printf "Will generate %d frames." max_frame;
  Core.Unix.mkdir_p output_dir;
  let rec loop frame =
    let time_ofs = frame // fps in
    match frame > max_frame with
    | true -> ()
    | false ->
        save_frame time_ofs
          (Core.Filename.concat output_dir
             (Caml.Printf.sprintf "%05d.png" frame));
        loop (frame + 1)
  in
  loop 0

