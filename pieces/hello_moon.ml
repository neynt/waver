(** "hello moon" sung via the IPA toy. Phones: h ɛ l oʊ m u n. *)
open Waver

let render output_file =
  let phrase =
    let open Ipa in
    [ { phone = silence; midi = 0; dur = 0.12 }
    ; { phone = glide_h; midi = 0; dur = 0.09 }
    ; { phone = vowel_eh; midi = 67; dur = 0.32 }
    ; { phone = liquid_l; midi = 67; dur = 0.14 }
    ; { phone = vowel_oh; midi = 72; dur = 0.55 }
    ; { phone = silence; midi = 0; dur = 0.16 }
    ; { phone = nasal_m; midi = 69; dur = 0.16 }
    ; { phone = vowel_oo; midi = 69; dur = 0.55 }
    ; { phone = nasal_n; midi = 65; dur = 0.32 }
    ; { phone = silence; midi = 0; dur = 0.3 }
    ]
  in
  let song = Ipa.sing phrase |> Signal.gain 1.5 in
  Wav.save ~sampling_rate:44100 output_file [ song ]
