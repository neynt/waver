(* The interesting part of this code was lifted from ocaml-numerical-analysis.
 * [MIT License] Copyright (C) 2015 Akinori ABE *)
open Core_kernel

let output_byte = Stdlib.output_byte
let output_string = Stdlib.output_string
let close_out = Stdlib.close_out

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
let output_le_s8f oc x = output_le_s8 oc (Int.of_float (x *. 128.))
let output_le_s16f oc x = output_le_s16 oc (Int.of_float (x *. 32768.))

let save ?(bit_depth = 16) ~sampling_rate filename (signals : Signal.t list) =
  let duration = List.map ~f:Signal.dur signals |> List.fold ~f:Float.max ~init:0. in
  let channels = List.length signals in
  let n = duration *. Float.of_int sampling_rate |> Float.round_up |> Int.of_float in
  let output_pt =
    match bit_depth with
    | 8 -> output_le_s8f
    | 16 -> output_le_s16f
    | _ -> invalid_arg "Invalid sampling bits (8 or 16 is supported)"
  in
  let block_size = channels * (bit_depth / 8) in
  let data_bytes = n * block_size in
  let oc = Stdio.Out_channel.create filename in
  output_string oc "RIFF";
  output_le_u31 oc (36 + data_bytes);
  (* #bytes of DATA chunk *)
  output_string oc "WAVE";
  output_string oc "fmt ";
  (* fmt chunk *)
  output_le_u31 oc 16;
  (* #bytes of fmt chunk *)
  output_le_u16 oc 1;
  (* format ID (linear PCM) *)
  output_le_u16 oc channels;
  (* #channels *)
  output_le_u31 oc sampling_rate;
  (* sampling rate *)
  output_le_u31 oc (block_size * sampling_rate);
  (* data speed *)
  output_le_u16 oc block_size;
  (* block size *)
  output_le_u16 oc bit_depth;
  (* bits per sample *)
  output_string oc "data";
  (* DATA chunk *)
  output_le_u31 oc data_bytes;
  (* #bytes of DATA chunk *)
  List.init n ~f:Fn.id
  |> List.iter ~f:(fun i ->
         let t = i // sampling_rate in
         List.iter signals ~f:(fun signal ->
             output_pt oc (Float.clamp_exn ~min:(-1.0) ~max:1.0 Signal.(signal.f t))));
  close_out oc

type wav_format = { channels : int; sampling_rate : int; bit_depth : int }
[@@deriving sexp_of]

type wav_chunk =
  | Format of wav_format
  | Data of { samples : string }
  (* keep this as a string for now, since it's hard to put the format in context *)
  | Unknown of { header : string; content : string }
[@@deriving sexp_of]

let load filename =
  let open Angstrom in
  let wav =
    let chunk_size = LE.any_int32 in
    let format_chunk =
      lift3
        (fun channels sampling_rate bit_depth ->
          let sampling_rate = Int32.to_int_exn sampling_rate in
          Format { channels; sampling_rate; bit_depth })
        (string "fmt "
        *> chunk_size
        *> LE.any_uint16
        (* always 1, for "PCM" format *) *> LE.any_uint16)
        (* num channels *)
        LE.any_int32 (* sampling rate *)
        (LE.any_int32
        (* data rate *) *> LE.any_uint16 (* bits per sample *)
        *> LE.any_uint16)
      (* bit depth *)
    in
    let data_chunk =
      string "data" *> chunk_size
      >>= (fun chunk_size -> take (Int32.to_int_exn chunk_size))
      >>| fun data -> Data { samples = data }
    in
    let unknown_chunk =
      lift2
        (fun header content -> Unknown { header; content })
        (take 4)
        (chunk_size >>= fun chunk_size -> take (Int32.to_int_exn chunk_size))
    in
    let chunk =
      choice
        ~failure_msg:"failed to parse any chunk"
        [ format_chunk; data_chunk; unknown_chunk ]
    in
    string "RIFF" *> chunk_size *> string "WAVE" *> many1 chunk
  in
  let file_content = Stdio.In_channel.read_all filename in
  let chunks = parse_string ~consume:All wav file_content |> Result.ok_or_failwith in
  (*List.iter chunks ~f:(function
    | Format { channels; sampling_rate; bit_depth } ->
        Core.print_s
          [%message
            "format" (channels : int) (sampling_rate : int) (bit_depth : int)]
    | Data _ -> Core.print_s [%message "data"]
    | Unknown { header; content = _ } ->
        Core.print_s [%message "header" (header : string)]);*)
  let { channels; sampling_rate; bit_depth } =
    List.find_map_exn chunks ~f:(function
        | Format x -> Some x
        | _ -> None)
  in
  let samples =
    List.find_map_exn chunks ~f:(function
        | Data { samples } -> Some samples
        | _ -> None)
  in
  let samples = Bigstring.of_string samples in
  let stride = bit_depth / 8 * channels in
  let length = Bigstring.length samples / stride in
  let samples_by_channel =
    match bit_depth with
    | 8 ->
      List.init channels ~f:(fun channel ->
          Array.init length ~f:(fun i ->
              Bigstring.unsafe_get_int8 samples ~pos:((i + channel) * stride) // 128))
    | 16 ->
      List.init channels ~f:(fun channel ->
          Array.init length ~f:(fun i ->
              Bigstring.unsafe_get_int16_le samples ~pos:((i + channel) * stride) // 32768))
    | 32 ->
      List.init channels ~f:(fun channel ->
          Array.init length ~f:(fun i ->
              (* TODO : maybe this doesn't in fact work? *)
              Bigstring.unsafe_get_int32_le samples ~pos:((i + channel) * stride)
              |> Obj.magic))
    | _ -> Core.failwith "unsupported bit depth"
  in
  List.map samples_by_channel ~f:(fun samples ->
      { Discrete_signal.samples; sample_rate = sampling_rate })
