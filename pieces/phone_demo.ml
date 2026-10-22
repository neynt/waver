(** Diagnostic — plays every IPA phone in isolation on A4, 0.4 s on / 0.3 s
    gap. Useful for hearing which phones are broken before assembling words. *)
open Base

open Waver

let render output_file =
  let note_dur = 0.4 in
  let gap = 0.3 in
  let phones_voiced =
    Ipa.
      [ "ah", vowel_ah
      ; "ee", vowel_ee
      ; "oo", vowel_oo
      ; "oh", vowel_oh
      ; "eh", vowel_eh
      ; "uh", vowel_uh
      ; "m", nasal_m
      ; "n", nasal_n
      ; "l", liquid_l
      ; "r", liquid_r
      ; "w", glide_w
      ; "y", glide_y
      ]
  in
  let phones_unvoiced =
    Ipa.[ "h", glide_h; "s", fric_s; "f", fric_f; "sh", fric_sh ]
  in
  let phones_stop = Ipa.[ "p", stop_p; "t", stop_t; "k", stop_k ] in
  let stop_dur = 0.08 in
  let fric_dur = 0.2 in
  let notes =
    List.concat
      [ List.concat_map phones_voiced ~f:(fun (_, p) ->
          [ Ipa.{ phone = p; midi = 69; dur = note_dur }
          ; Ipa.{ phone = silence; midi = 0; dur = gap }
          ])
      ; List.concat_map phones_unvoiced ~f:(fun (_, p) ->
          [ Ipa.{ phone = p; midi = 0; dur = fric_dur }
          ; Ipa.{ phone = silence; midi = 0; dur = gap }
          ])
      ; List.concat_map phones_stop ~f:(fun (_, p) ->
          [ Ipa.{ phone = p; midi = 0; dur = stop_dur }
          ; Ipa.{ phone = silence; midi = 0; dur = gap }
          ])
      ]
  in
  let song = Ipa.sing notes |> Signal.gain 1.0 in
  Wav.save ~sampling_rate:44100 output_file [ song ]
