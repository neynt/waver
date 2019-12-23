open Base

let midi_demo input_file output_file =
  let midi_file = Midi.read_file input_file in
  let time_scale = 1. in
  let offsets_and_signals =
    List.map (List.concat midi_file.tracks)
      ~f:(fun { midi; time; dur; velo; _ } ->
        ( time *. time_scale,
          Instrument.blip midi (dur *. time_scale)
            ~velo:(Float.of_int velo /. 255.) ))
  in
  Wav.save ~sampling_rate:44100 output_file
    [ Signal.render offsets_and_signals ]

let render_cmd =
  Core.Command.basic ~summary:"Render a midi file to a wav."
    Core.Command.Let_syntax.(
      let%map_open input_file = anon ("input_file" %: string)
      and output_file = anon ("output_file" %: string) in
      fun () -> midi_demo input_file output_file)

let frame_cmd =
  Core.Command.basic ~summary:"Draw a single frame."
    Core.Command.Let_syntax.(
      let%map_open input_file = anon ("input_file" %: string)
      and output_file = anon ("output_file" %: string)
      and time =
        flag "-time"
          (optional_with_default 0. float)
          ~doc:"Time in the song to render."
      in
      fun () ->
        let _max_time, save_frame = Video.frame_demo input_file in
        save_frame time output_file)

let video_cmd =
  Core.Command.basic ~summary:"Create the frames for a video."
    Core.Command.Let_syntax.(
      let%map_open input_file = anon ("input_file" %: string)
      and output_dir = anon ("output_dir" %: string)
      and fps = flag "-fps" (required int) ~doc:" frames per second" in
      fun () ->
        let max_time, save_frame = Video.frame_demo input_file in
        Video.video save_frame max_time output_dir fps)

let video_spectrum_cmd =
  Core.Command.basic ~summary:"Create the frames for a spectral video."
    Core.Command.Let_syntax.(
      let%map_open input_file = anon ("input_file" %: string)
      and output_dir = anon ("output_dir" %: string)
      and fps = flag "-fps" (required int) ~doc:" frames per second" in
      fun () ->
        let save_frame = Vivid.spectrum input_file in
        Video.video save_frame 30. output_dir fps)

let trigger_cmd =
  Core.Command.basic ~summary:"Called on recompile."
    Core.Command.Let_syntax.(
      let%map_open output_file = anon ("output_file" %: string) in
      fun () -> Vivid.main output_file)

let render_adrestia_sfx_cmd =
  Core.Command.basic ~summary:"Render Adrestia's sound effects to a directory."
    Core.Command.Let_syntax.(
      let%map_open output_dir = anon ("output_dir" %: string) in
      fun () -> Adrestia_sfx.render output_dir)

let command =
  Core.Command.group ~summary:"Do cool things with midis"
    [
      ("render", render_cmd);
      ("frame", frame_cmd);
      ("video", video_cmd);
      ("video-spectrum", video_spectrum_cmd);
      ("trigger", trigger_cmd);
      ("render-adrestia-sfx", render_adrestia_sfx_cmd);
    ]

let () = Core.Command.run command
