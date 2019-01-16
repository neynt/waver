(* little-endian unsigned 16-bit integer *)
let output_le_u16 oc n =
  output_byte oc (n land 0xff);
  output_byte oc ((n land 0xff00) lsr 8)

(* little-endian unsigned 31-bit integer *)
let output_le_u31 oc n =
  output_le_u16 oc (n land 0xffff);
  output_le_u16 oc ((n land 0x7fff0000) lsr 16)

let output_le_s8 oc n = output_byte oc (if n >= 0 then n else n + 0x100)
let output_le_s16 oc n = output_le_u16 oc (if n >= 0 then n else n + 0x10000)
let output_le_s8f oc x = output_le_s8 oc (int_of_float (x *. 128.))
let output_le_s16f oc x = output_le_s16 oc (int_of_float (x *. 32768.))

let save ?(sampling_bits = 16) ~sampling_rate filename (signals : Signal.t list) =
  let duration =
    List.map Signal.dur signals
    |> List.fold_left max 0.
  in
  let channels = List.length signals in
  let n = (duration |> ceil |> int_of_float) * sampling_rate in
  let output_pt = match sampling_bits with
    | 8 -> output_le_s8f
    | 16 -> output_le_s16f
    | _ -> invalid_arg "Invalid sampling bits (8 or 16 is supported)" in
  let block_size = channels * (sampling_bits / 8) in
  let data_bytes = n * block_size in
  let oc = open_out_bin filename in
  output_string oc "RIFF";
  output_le_u31 oc (36 + data_bytes); (* #bytes of DATA chunk *)
  output_string oc "WAVEfmt ";
  output_le_u31 oc 16; (* #bytes of FMT chunk *)
  output_le_u16 oc 1; (* format ID (linear PCM) *)
  output_le_u16 oc channels; (* #channels *)
  output_le_u31 oc sampling_rate; (* sampling rate *)
  output_le_u31 oc (block_size * sampling_rate); (* data speed *)
  output_le_u16 oc block_size; (* block size *)
  output_le_u16 oc sampling_bits; (* #bits per one sample *)
  output_string oc "data"; (* DATA chunk *)
  output_le_u31 oc data_bytes; (* #bytes of DATA chunk *)

  List.iter
    (fun i ->
      let t = float_of_int i /. float_of_int sampling_rate in
      List.iter (fun signal -> output_pt oc (Signal.at signal t)) signals)
    (List.init n (fun i -> i));
  close_out oc
;;
