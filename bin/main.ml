open Aoc

let () =
  let arg: string = Array.get (Sys.argv) 1 in
  Dayone.part_one arg |> print_int |> print_newline
