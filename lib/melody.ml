open Core

(* Melodies relative to a scale. *)
type scale_degree = { index : int; accidental : int } [@@deriving sexp]

type note = { content : [ `Pitch of scale_degree | `Rest ]; length : int }
[@@deriving sexp]

type t = note list [@@deriving sexp]

(* holy shit armstrong is strong af *)
let of_text_notation =
  let open Angstrom in
  let spaces =
    skip_while (function
        | ' ' | '\n' -> true
        | _ -> false)
  in
  let lex p = p <* spaces in
  let sharp = char '#' *> return `Sharp in
  let flat = char 'b' *> return `Flat in
  let accidental = option `Natural (sharp <|> flat) in
  let extender =
    take_while (function
        | '-' -> true
        | ' ' -> true
        | _ -> false)
    >>| String.count ~f:(function
            | '-' -> true
            | _ -> false)
  in
  let integer =
    take_while1 (function
        | '0' .. '9' -> true
        | _ -> false)
    >>| int_of_string
  in
  let note =
    lift3
      (fun index accidental extender ->
        let accidental =
          match accidental with
          | `Sharp -> 1
          | `Flat -> -1
          | `Natural -> 0
        in
        { content = `Pitch { index; accidental }; length = extender + 1 })
      integer
      accidental
      extender
  in
  let melody = spaces *> many (lex note) in
  fun s -> parse_string ~consume:All melody s |> Result.ok_or_failwith
