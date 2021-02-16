open Core_kernel
module Compo = Composition.Make ()

module Ukulele_tab = struct
  module Line = struct
    type note = { pos : int; note : int; duration : int }
    type t = note list

    module Parser = struct
      type t = { current_note : note option }

      let empty = { current_note = None }
    end

    let parse line =
      let notes = Queue.create () in
      let maybe_add_note (state : Parser.t) =
        match state.current_note with
        | None -> ()
        | Some note ->
          let note = { note with pos = note.pos - 1; duration = note.duration + 1 } in
          Queue.enqueue notes note
      in
      let final_note =
        String.to_list line
        |> List.foldi ~init:Parser.empty ~f:(fun i state c ->
               match c with
               | '-' ->
                 maybe_add_note state;
                 { current_note = None }
               | '=' ->
                 { current_note =
                     (let%map.Option { pos; note; duration } = state.current_note in
                      { pos; note; duration = duration + 1 })
                 }
               | '0' .. '9' ->
                 let digit = Int.of_string (String.of_char c) in
                 (match state.current_note with
                 | Some { pos = _; note; duration = 0 } ->
                   { current_note =
                       Some { pos = i; note = (note * 10) + digit; duration = 0 }
                   }
                 | _ ->
                   maybe_add_note state;
                   { current_note = Some { pos = i; note = digit; duration = 0 } })
               | _ -> state)
      in
      maybe_add_note final_note;
      Queue.to_list notes
  end
end

let render output_file =
  let open Compo in
  let open Instrument in
  bpm := 480.;
  let play beats note = play beats (blaap note (bt beats)) in
  let play_ukulele tab =
    String.strip tab
    |> String.split_lines
    |> List.zip_exn [ 69; 64; 60; 55 ]
    |> List.map ~f:(fun (midi_offset, line) () ->
           let notes = Ukulele_tab.Line.parse line in
           List.iter notes ~f:(fun { pos; note; duration } ->
               at pos (blip (note + midi_offset) (bt duration)));
           List.map notes ~f:(fun { pos; duration; _ } -> pos + duration)
           |> List.max_elt ~compare:Int.compare
           |> Option.iter ~f:advance)
    |> together
  in
  start_here ();
  play_ukulele
    {|
|------------------------|
|------------0===========|
|--------0=2=------------|
|2===2=4=----------------|
  |};
  play_ukulele
    {|
|------------------------|
|0===------0=------------|
|--------0=--2===========|
|----2=4=----------------|
  |};
  play_ukulele
    {|
|------------------------|
|------------------------|
|2===----0=2=0===--------|
|----2=4=--------4===2===|
  |};
  play_ukulele
    {|
|------------------------|
|--------------------0=--|
|--------0=2=----------2=|
|2=====4=----4=======----|
  |};
  play_ukulele
    {|
|--------------------0=--|
|------------0===------4=|
|0===----0=2=------------|
|----2=4=--------2===----|
  |};
  play_ukulele
    {|
|0===--------------------|
|----------0=--------3=1=|
|--------0=--2===--------|
|----2=4=--------0===----|
  |};
  play_ukulele
    {|
|------------------------|
|3===--------------------|
|--------0=2=0===--------|
|----2=4=--------4===2===|
  |};
  play_ukulele
    {|
|------------------------|
|--------0===----------0=|
|--------------------0=--|
|1=======----2=====4=----|
  |};
  play_ukulele
    {|
|0=======----------------|
|------------------------|
|------------------------|
|------------------------|
  |};
  end_here ();
  let _don't () =
    together [ (fun () -> play 2 72); (fun () -> play 2 76) ];
    together [ (fun () -> play 2 70); (fun () -> play 2 77) ]
  in
  let song = Signal.render (result ()) in
  Wav.save ~sampling_rate:44100 output_file [ song ]
