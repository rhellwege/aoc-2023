open Aoc

let () =
  let arg: string = Array.get (Sys.argv) 1 in
  Dayone.process arg |> print_int |> print_newline
