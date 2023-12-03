open Aoc

let () =
  if Array.length Sys.argv < 2 then print_endline "Please provide a file path as an argument."
  else
    let arg: string = Array.get (Sys.argv) 1 in
  Daythree.process arg |> print_int |> print_newline
