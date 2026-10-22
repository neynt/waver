(** "sushi" — sung two-syllable demo with both sibilants (s, sh). *)
open Waver

let render output_file =
  let phrase =
    let open Ipa in
    [ { phone = silence; midi = 0; dur = 0.12 }
    ; { phone = fric_s; midi = 0; dur = 0.18 }
    ; { phone = vowel_oo; midi = 67; dur = 0.45 }
    ; { phone = fric_sh; midi = 0; dur = 0.20 }
    ; { phone = vowel_ee; midi = 71; dur = 0.50 }
    ; { phone = silence; midi = 0; dur = 0.30 }
    ]
  in
  let song = Ipa.sing phrase |> Signal.gain 1.5 in
  Wav.save ~sampling_rate:44100 output_file [ song ]
