type note =
  { time: int
  ; midi: int
  ; chan: int
  ; velo: int
  }

type t =
  { time_unit: float
  ; tracks: note list list
  }

let input_be_u32 ic =
  let res = 0x1000000 * input_byte ic in
  let res = res + 0x10000 * input_byte ic in
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
    let cur = cur * 0x100 + (b land 0x7f) in
    if b lsr 7 > 0 then aux cur
    else cur
  in
  aux 0
;;

let do_stuff () =
  let ic = open_in_bin "ib-memory.mid" in
  if ((really_input_string ic 4) <> "MThd") then failwith "Not MIDI format";
  let _chunk_len = input_be_u32 ic in
  let midi_format = input_be_u16 ic in
  let num_tracks = input_be_u16 ic in
  let division = input_be_u16 ic in
  Printf.printf "Format: %d - Tracks: %d - Division: %d\n" midi_format num_tracks division;

  let tracks = ref [] in
  try
    let read_chunk ic =
      if (really_input_string ic 4 <> "MTrk") then failwith "Found a non-MTrk MIDI chunk";
      let chunk_len = input_be_u32 ic in
      Printf.printf "Found track of Length %d\n" chunk_len;
      let running_status = ref 0 in
      let time = ref 0 in
      let fin = ref false in
      let notes = ref [] in
      while not !fin do
        let time_delta = input_vlq ic in
        time := !time + time_delta;
        let status = input_byte ic in
        (match (status lsr 7) with
        | 0 -> seek_in ic (pos_in ic - 1);
        | _ -> running_status := (status lsr 4) land 0x7);
        match !running_status, status land 0xf with
        | 0, chan ->
          let midi = input_byte ic in
          let _velo = input_byte ic in
          Printf.printf "[%d] Note %d off (channel %d)\n" !time midi chan;
        | 1, chan ->
          let midi = input_byte ic in
          let velo = input_byte ic in
          notes := List.cons { time = !time; midi; chan; velo } !notes;
          Printf.printf "[%d] Note %d on (channel %d)\n" !time midi chan;
        | 2, chan ->
          let midi = input_byte ic in
          let _velo = input_byte ic in
          Printf.printf "[%d] Note %d bottomed out (channel %d)\n" !time midi chan;
        | 3, _chan ->
          let controller = input_byte ic in
          let new_value = input_byte ic in
          Printf.printf "[%d] Control change of %d to %d\n" !time controller new_value;
        | 4, _chan ->
          let program = input_byte ic in
          Printf.printf "[%d] Program change to %d\n" !time program;
        | 7, 15 ->
          (* Meta event *)
          let meta = input_byte ic in
          let len = input_vlq ic in
          let content = really_input_string ic len in
          (match meta with
            | 0 -> Printf.printf "[%d] Meta sequence number\n" !time;
            | 1 | 8 | 9 | 10 | 12 -> Printf.printf "[%d] Text-type meta 0x%x (%s)\n" !time meta content;
            | 0x58 -> Printf.printf "[%d] Meta time signature\n" !time;
            | 0x59 -> Printf.printf "[%d] Meta key signature\n" !time;
            | 0x2f ->
              Printf.printf "==END OF TRACK==\n";
              fin := true;
            | _ -> Printf.printf "[%d] Unknown meta 0x%x of length %d\n" !time meta len;
            ());
        | _, chan ->
          Printf.printf "Unknown status id %d (%d, %d)\n" status !running_status chan;
          failwith "exiting"
      done;
      Printf.printf "Finished reading chunk!!\n";
      !notes
    in
    let rec chunk_loop () =
      tracks := List.cons (read_chunk ic) !tracks;
      chunk_loop ()
    in
    chunk_loop ()
  with End_of_file -> ();

  let result =
    { time_unit = float_of_int division
    ; tracks = !tracks
    }
  in
  result
;;
