open Base

type note =
  { time: float
  ; dur: float
  ; midi: int
  ; chan: int
  ; velo: int
  }

type t =
  { tracks: note list list
  }

let input_byte = Stdlib.input_byte;;
let really_input_string = Stdlib.really_input_string;;
let printf = Caml.Printf.printf;;
let seek_in = Stdlib.seek_in;;
let pos_in = Stdlib.pos_in;;

let input_be_u32 ic =
  let res = 0x1000000 * input_byte ic in
  let res = res + 0x10000 * input_byte ic in
  let res = res + 0x100 * input_byte ic in
  res + input_byte ic
;;

let input_be_u24 ic =
  let res = 0x10000 * input_byte ic in
  let res = res + 0x100 * input_byte ic in
  res + input_byte ic
;;

let input_be_u16 ic =
  let res = 0x100 * input_byte ic in
  res + input_byte ic
;;

let input_vlq ic =
  let rec aux cur =
    let b = input_byte ic in
    let cur = cur * 0x80 + (b land 0x7f) in
    if b lsr 7 > 0 then aux cur
    else cur
  in
  aux 0
;;

let read_file filename =
  let ic = Stdlib.open_in_bin filename in
  if (String.(<>) (really_input_string ic 4) "MThd") then failwith "Not MIDI format";
  let _chunk_len = input_be_u32 ic in
  let midi_format = input_be_u16 ic in
  let num_tracks = input_be_u16 ic in
  let division = input_be_u16 ic in
  (match division lsr 14 with
  | 1 -> failwith "Negative SMPTE format not supported"
  | _ -> ());
  printf "Format: %d - Tracks: %d - Division: %d\n" midi_format num_tracks division;
  let division = Float.of_int division in

  let tracks = ref [] in
  try
    let read_chunk ic tempo_map =
      if (String.(<>) (really_input_string ic 4) "MTrk") then failwith "Found a non-MTrk MIDI chunk";
      let chunk_len = input_be_u32 ic in
      printf "==BEGIN TRACK (length %d)==\n" chunk_len;
      let running_status = ref 0 in
      let tick = ref 0 in
      let fin = ref false in
      let tempo_map = ref tempo_map in
      let pressed = Hashtbl.create (module Int) in
      let notes = ref [] in
      while not !fin do
        let ticks = input_vlq ic in
        tick := !tick + ticks;
        let _, timing_fn = Option.value_exn (Map.closest_key !tempo_map `Less_than !tick) in
        let time = timing_fn !tick in
        let status = input_byte ic in
        (match (status lsr 7) with
        | 0 -> seek_in ic (pos_in ic - 1);
        | _ -> running_status := (status lsr 4) land 0x7);
        match !running_status, status land 0xf with
        | 0, _chan ->
          let midi = input_byte ic in
          let velo = input_byte ic in
          printf "[%f] Note off (%d %d)\n" time midi velo;
          ()
        | 1, chan ->
          let midi = input_byte ic in
          let velo = input_byte ic in
          (match velo with
          | 0 ->
            (* printf "[%f] Note 'off' (%d)\n" time midi; *)
            let start_time, velo = Hashtbl.find_exn pressed midi in
            notes := List.cons { time = start_time; dur = time -. start_time; midi; chan; velo } !notes;
          | _ ->
            (* printf "[%f] Note on (%d %d)\n" time midi velo; *)
            ());
          Hashtbl.set pressed ~key:midi ~data:(time, velo);
          ()
        | 2, _chan ->
          let midi = input_byte ic in
          let velo = input_byte ic in
          printf "[%f] Aftertouch (%d %d)\n" time midi velo;
          ()
        | 3, _chan ->
          let _controller = input_byte ic in
          let _value = input_byte ic in
          (* printf "[%f] Controller change (%d %d)\n" time controller value; *)
          ()
        | 4, _chan ->
          let program = input_byte ic in
          printf "[%f] Program change (%d)\n" time program;
          ()
        | 7, 15 ->
          (* Meta event *)
          let meta = input_byte ic in
          let len = input_vlq ic in
          let content = really_input_string ic len in
          (match meta with
            | 0 -> printf "[%f] Meta sequence number\n" time;
            | 1 | 8 | 9 | 10 | 12 -> printf "[%f] Text-type meta 0x%x (%s)\n" time meta content;
            | 0x51 ->
              seek_in ic (pos_in ic - 3);
              let tempo = input_be_u24 ic in
              printf "[%f] Meta set tempo to %d\n" time tempo;
              let start_time = time in
              let start_tick = !tick in
              tempo_map := Map.add_exn !tempo_map ~key:start_tick ~data:(fun tick ->
                start_time +. Float.of_int ((tick - start_tick) * tempo) /. (division *. 1_000_000.));
            | 0x58 -> printf "[%f] Meta time signature\n" time;
            | 0x59 -> printf "[%f] Meta key signature\n" time;
            | 0x2f ->
              printf "==END OF TRACK==\n";
              fin := true;
            | _ -> printf "[%f] Unknown meta 0x%x of length %d\n" time meta len;
            ());
        | _, chan ->
          printf "[%f] Unknown status id %d (%d, %d)\n" time status !running_status chan;
          failwith "exiting"
      done;
      !notes, !tempo_map
    in
    let tempo_map = Map.empty (module Int) in
    let tempo_map =
      Map.add_exn tempo_map
        ~key:Int.min_value
        ~data:(fun ticks -> Float.of_int ticks *. 500_000. /. division /. 1_000_000.)
    in
    let rec chunk_loop tempo_map =
      let track, tempo_map = read_chunk ic tempo_map in
      tracks := List.cons track !tracks;
      chunk_loop tempo_map
    in
    chunk_loop tempo_map
  with End_of_file -> ();

  let result =
    { tracks = !tracks
    }
  in
  result
;;
