open Base
module Compo = Composition.Make ()
module FFT = Fftw3.D

let fft data =
  let dim = Array.length data in
  let x = FFT.Array1.create FFT.complex Bigarray.c_layout dim in
  let y = FFT.Array1.create FFT.complex Bigarray.c_layout dim in
  let plan = FFT.Array1.dft FFT.Forward x y in
  Array.iteri data ~f:(fun i s -> x.{i} <- { re = s; im = 0. });
  FFT.exec plan;
  Array.init dim ~f:(fun i -> y.{i})

let plot_signal
    ?(lower = -1.)
    ?(upper = 1.)
    ?(resolution = 44100)
    ?(color = Fn.const (Draw.rgb 255 255 255))
    (signal : Signal.t)
    w
    h
    output_file
  =
  let total_samples = Float.iround_up_exn (Int.to_float resolution *. signal.dur) in
  let image = new OImages.rgb24 w h in
  Draw.fill image (Draw.rgb 0 0 0);
  let y_of_s s =
    Float.iround_nearest_exn ((upper -. s) *. (Int.to_float h /. (upper -. lower)))
  in
  let ix x = total_samples * x / w in
  let rec loop x prev_s =
    if x >= w
    then ()
    else (
      (* one sample per pixel for now *)
      let pixel_samples =
        let start_x = ix x
        and end_x = ix (x + 1) in
        prev_s
        :: List.init (end_x - start_x) ~f:(fun dx ->
               let t = (start_x + dx) // resolution in
               signal.f t)
      in
      let y1 =
        List.min_elt pixel_samples ~compare:Float.compare |> Option.value_exn |> y_of_s
      and y0 =
        List.max_elt pixel_samples ~compare:Float.compare |> Option.value_exn |> y_of_s
      in
      let h = y1 - y0 + 1 in
      Draw.rect image x y0 1 h (color x);
      loop (x + 1) (List.last_exn pixel_samples))
  in
  loop 0 (signal.f (-1 // resolution));
  image#save output_file None []

let _main _input_file output_file =
  let signal = Signal.sine 800. |> Signal.crop 0.1 |> Signal.gain 0.0005 in
  Wav.save ~sampling_rate:44100 output_file [ signal ];
  let discrete_signal = Converter.discretize signal ~sample_rate:44100 in
  let fft_signal =
    { discrete_signal with
      samples = fft discrete_signal.samples |> Array.map ~f:Caml.Complex.norm
    }
    |> Converter.zero_order_hold
  in
  plot_signal fft_signal 1200 900 "signal.png"

let spectrum input_file =
  let dur = 0.2 in
  let sample_rate = 44100 in
  let samples = Float.iround_up_exn (Int.to_float sample_rate *. dur) in
  let channels = Wav.load input_file in
  let result = List.map ~f:Converter.zero_order_hold channels in
  let save_frame time_ofs filename =
    let signal =
      List.hd_exn result
      |> Signal.delay (-.time_ofs)
      |> Signal.crop (samples // sample_rate)
    in
    let discrete_signal = Converter.discretize signal ~sample_rate in
    let fft_signal =
      { discrete_signal with
        samples = fft discrete_signal.samples |> Array.map ~f:Caml.Complex.norm
      }
      |> Converter.zero_order_hold
      |> Signal.map ~f:(fun s -> Float.log1p s)
    in
    let fft_signal = fft_signal |> Signal.crop (fft_signal.dur /. 2.) in
    plot_signal ~lower:(-0.1) ~upper:5. fft_signal 1200 900 filename
  in
  save_frame

let spectrum2 input_file =
  let dur = 0.10 in
  let sample_rate = 44100 in
  let samples = Float.iround_up_exn (Int.to_float sample_rate *. dur) in
  let channels = Wav.load input_file in
  let result = List.map ~f:Converter.zero_order_hold channels in
  let save_frame time_ofs filename =
    let signal =
      List.hd_exn result
      |> Signal.delay (-.time_ofs)
      |> Signal.crop (samples // sample_rate)
    in
    let discrete_signal = Converter.discretize signal ~sample_rate in
    let fft_length = Array.length discrete_signal.samples in
    let fft_signal =
      { discrete_signal with
        samples = fft discrete_signal.samples |> Array.map ~f:Caml.Complex.norm
      }
      |> Converter.zero_order_hold
      |> Signal.map ~f:(fun s -> Float.log1p s)
    in
    let fft_signal = fft_signal |> Signal.crop (fft_signal.dur /. 2.) in
    let freq_to_note = Temperament.equal_inv 440. in
    plot_signal
      ~lower:(-0.1)
      ~upper:5.
      ~color:(fun x ->
        let freq = (x // 1920 *. Float.of_int fft_length) +. 1. in
        let octave_position = freq_to_note freq /. 12. in
        { Chroma.h = Float.mod_float (octave_position +. 100.) 1.; s = 1.; v = 1. }
        |> Chroma.hsv_to_rgb
        |> Chroma.rgb_to_color)
      fft_signal
      1920
      900
      filename
  in
  save_frame

let main output_file =
  let open Compo in
  let open Instrument in
  bpm := 200.;
  let melody = melody ~instrument:(fun note dur -> blip note dur) in
  let scale = Scale.minor 60 in
  let _bass_scale = Scale.shift_octaves scale (-2) in
  let chord1 = Scale.subscale scale [ 0; 1; 2; 3 ] in
  let chord2 = Scale.subscale scale [ 1; 2; 3; 4 ] in
  let rec times n f =
    if n <= 0
    then ()
    else (
      f ();
      times (n - 1) f)
  in
  push_mark ();
  times 2 (fun () -> melody chord1 {| 0 1 2 |});
  times 2 (fun () -> melody chord2 {| 0 1 2 |});
  let song = Signal.render (result ()) in
  Wav.save ~sampling_rate:44100 output_file [ song ]
