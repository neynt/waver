open Core_kernel
open Waver
open Waver_pieces
module Command = Core.Command

let fps_flag =
  let open Command.Param in
  flag "-fps" (required int) ~doc:" frames per second"

let time_flag =
  let open Command.Param in
  flag "-time" (optional_with_default 0. float) ~doc:"t Time in the song to render."

let midi_demo input_file output_file =
  let midi_file = Midi.read_file input_file in
  let time_scale = 1. in
  let offsets_and_signals =
    List.map (List.concat midi_file.tracks) ~f:(fun { midi; time; dur; velo; _ } ->
        ( time *. time_scale
        , Instrument.blip midi (dur *. time_scale) ~velo:(Float.of_int velo /. 255.) ))
  in
  Wav.save ~sampling_rate:44100 output_file [ Signal.render offsets_and_signals ]

let render_midi_cmd =
  Command.basic
    ~summary:"Render a midi file to a wav."
    [%map_open.Command
      let input_file = anon ("input_file" %: string)
      and output_file = anon ("output_file" %: string) in
      fun () -> midi_demo input_file output_file]

let render_adrestia_sfx_cmd =
  Command.basic
    ~summary:"Render Adrestia's sound effects to a directory."
    [%map_open.Command
      let output_dir = anon ("output_dir" %: string) in
      fun () -> Adrestia_sfx.render output_dir]

let render_workspace_cmd =
  Command.basic
    ~summary:"Render whatever is produced by workspace.ml"
    [%map_open.Command
      let output_file = anon ("output_file" %: string) in
      fun () -> Workspace.render output_file]

module Spectrum = struct
  let frame_cmd =
    Command.basic
      ~summary:"Create the frames for a spectral video."
      [%map_open.Command
        let input_file = anon ("input_file" %: string)
        and frame_output_file = anon ("frame_output_file" %: string)
        and wav_output_file = anon ("wav_output_file" %: string)
        and time = time_flag in
        fun () ->
          let channels = Wav.load input_file in
          let sampling_rate = List.hd_exn channels |> Discrete_signal.sample_rate in
          let () =
            channels
            |> List.map ~f:Converter.zero_order_hold
            |> List.map ~f:(Signal.delay (-.time))
            |> List.map ~f:(Signal.crop 0.3)
            |> Wav.save ~sampling_rate wav_output_file
          in
          let save_frame = Spectrogram.spectrum2 input_file in
          save_frame time frame_output_file]

  let render_cmd =
    Command.basic
      ~summary:"Create the frames for a spectral video."
      [%map_open.Command
        let input_file = anon ("input_file" %: string)
        and output_dir = anon ("output_dir" %: string)
        and fps = fps_flag in
        fun () ->
          let save_frame = Spectrogram.spectrum2 input_file in
          Video.video save_frame 30. output_dir fps]

  let command =
    Command.group ~summary:"Render spectrum" [ "render", render_cmd; "frame", frame_cmd ]
end

module Piano_roll = struct
  let frame_cmd =
    Command.basic
      ~summary:"Draw a single frame."
      [%map_open.Command
        let input_file = anon ("input_file" %: string)
        and output_file = anon ("output_file" %: string)
        and time = time_flag in
        fun () ->
          let _max_time, save_frame = Video.frame_demo input_file in
          save_frame time output_file]

  let render_cmd =
    Command.basic
      ~summary:"Create the frames for a video."
      [%map_open.Command
        let input_file = anon ("input_file" %: string)
        and output_dir = anon ("output_dir" %: string)
        and fps = fps_flag in
        fun () ->
          let max_time, save_frame = Video.frame_demo input_file in
          Video.video save_frame max_time output_dir fps]

  let command =
    Command.group
      ~summary:"Render piano roll"
      [ "render", render_cmd; "frame", frame_cmd ]
end

let command =
  Command.group
    ~summary:"make waves"
    [ "render-midi", render_midi_cmd
    ; "ukulele-chord-shapes", Ukulele_chord_shapes.command
    ; "piano-roll", Piano_roll.command
    ; "spectrum", Spectrum.command
    ; "render-adrestia-sfx", render_adrestia_sfx_cmd
    ; "render-workspace", render_workspace_cmd
    ]

let () = Command.run command
