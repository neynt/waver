open Core_kernel
open Waver
module Compo = Composition.Make ()

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
  play_ukulele
    {|
|0===========------------|
|------------4===========|
|--------0=2=4===========|
|2===2=4=----------------|
  |};
  play_ukulele
    {|
|------------------------|
|3===========2===========|
|4===----0=4=2===========|
|----2=4=----------------|
  |};
  play_ukulele
    {|
|------------------------|
|1===========0===========|
|2===----0=2=0===--------|
|----2=4=--------4===2===|
  |};
  play_ukulele
    {|
|------------------------|
|------------0===========|
|3=======0=2=--------4=2=|
|2=====4=----4=======----|
  |};
  play_ukulele
    {|
|0===========--------0=--|
|------------4=========4=|
|0===----0=2=4===--------|
|----2=4=--------2===----|
  |};
  play_ukulele
    {|
|0===--------------------|
|3=========0=2=======3=1=|
|--------0=--2===--------|
|----2=4=--------0===----|
  |};
  play_ukulele
    {|
|------------------------|
|3===--------0===========|
|5=======0=2=0===--------|
|----2=4=--------4===2===|
  |};
  play_ukulele
    {|
|------------------------|
|0=======0===----------0=|
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
  play_ukulele
    {|
|------------------------|
|----0=====--0=3=1=0=--0=|
|1=2=----0=2=--------2=--|
|------2=----------------|
  |};
  play_ukulele
    {|
|------------------------|
|1===--------1=1=0===----|
|----2===--2=--------0=2=|
|----0===4=--------------|
  |};
  start_here ();
  play_ukulele
    {|
|------------------------|
|0===0===----0=3=1=0=--0=|
|----------1=--------2=--|
|----2===4=--------------|
  |};
  play_ukulele
    {|
|------------------------|
|1===------------0=======|
|----2===----0=2=--------|
|--------2=4=------------|
  |};
  end_here ();
  together [ (fun () -> play 2 72); (fun () -> play 2 76) ];
  together [ (fun () -> play 2 70); (fun () -> play 2 77) ];
  let song = Signal.render (result ()) in
  Wav.save ~sampling_rate:44100 output_file [ song ]