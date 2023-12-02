open Util

type subgame = {
  red: int;
  green: int;
  blue: int
}

type game = {
  id: int;
  set: subgame list
}

let parse_subgame line =
  let parse_cube line =
    let (n, line) = get_opt @@ parse_int line in
    let (_, line) = get_opt @@ parse_whitespace line in
    let (color, line) = get_opt @@ parse_word line in
    Some ((n, color), line)
  in
  let rec aux acc line =
    let cube = parse_cube line in
    match cube with
    | None -> None
    | Some ((n, color), line) -> (
      let acc = match color with
      | "red" -> {acc with red = n}
      | "green" -> {acc with green = n}
      | "blue" -> {acc with blue = n}
      | _ -> acc
      in
      let comma = expect_string ", " line in
      match comma with
      | None -> Some (acc, line)
      | Some (_, line) -> aux acc line
    )
  in
  aux {red = 0; green = 0; blue = 0} line

let parse_game line =
  let (_, line) = get_opt @@ expect_string "Game " line in
  let (_, line) = get_opt @@ parse_int line in
  let (_, line) = get_opt @@ expect_string ": " line in
  line

let process infile = 
  let lines = Util.seq_of_filename infile in
  let () = lines |> Seq.iter (fun line -> print_endline line;) in
  1
