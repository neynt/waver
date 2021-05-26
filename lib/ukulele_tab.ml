open Core_kernel

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
