open Core_kernel

let main () =
  let strings = [ [ 9; 10; 11; 0 ]; [ 4; 5; 6; 7 ]; [ 0; 1; 2; 3 ]; [ 7; 8; 9; 10 ] ] in
  let _notes = List.map strings ~f:(Fn.flip List.nth 0) in
  print_endline "hey"

let command =
  Command.basic
    ~summary:"Print ukulele chord shapes"
    [%map_open.Command
      let () = return () in
      fun () -> main ()]
