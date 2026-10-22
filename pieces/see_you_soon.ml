(** "see you soon" — short sentence: sibilants, glide-y, nasal n. *)
open Waver

let render output_file =
  let phrase =
    let open Ipa in
    [ { phone = silence; midi = 0; dur = 0.10 }
    ; { phone = fric_s; midi = 0; dur = 0.16 }
    ; { phone = vowel_ee; midi = 67; dur = 0.32 }
    ; { phone = silence; midi = 0; dur = 0.06 }
    ; { phone = glide_y_to oo_formants; midi = 69; dur = 0.10 }
    ; { phone = vowel_oo; midi = 69; dur = 0.30 }
    ; { phone = silence; midi = 0; dur = 0.08 }
    ; { phone = fric_s; midi = 0; dur = 0.18 }
    ; { phone = vowel_oo; midi = 65; dur = 0.42 }
    ; { phone = nasal_n; midi = 64; dur = 0.22 }
    ; { phone = silence; midi = 0; dur = 0.35 }
    ]
  in
  let song = Ipa.sing phrase |> Signal.gain 1.5 in
  Wav.save ~sampling_rate:44100 output_file [ song ]
