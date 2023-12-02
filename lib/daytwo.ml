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
      match expect_string ", " line with
      | None -> Some (acc, line)
      | Some (_, line) -> aux acc line
    )
  in
  aux {red = 0; green = 0; blue = 0} line

let parse_game line =
  let (_, line) = get_opt @@ expect_string "Game " line in
  let (id, line) = get_opt @@ parse_int line in
  let (_, line) = get_opt @@ expect_string ": " line in
  let rec aux acc line =
    match parse_subgame line with
    | None -> acc
    | Some (sg, line) -> (
        match expect_string "; " line with
        | None -> sg :: acc
        | Some (_, line) -> aux (sg :: acc) line
      )
  in 
  let set = aux [] line in
  {id = id; set = set}

let game_possible (g: game) (original: subgame) : bool =
  List.for_all (fun x -> 
    x.red <= original.red && x.green <= original.green && x.blue <= original.blue
  ) g.set

let process infile = 
  let lines = Util.seq_of_filename infile in
  lines 
    |> Seq.filter_map (fun line -> 
      match (line |> parse_game) with
    | g when game_possible g {red=12;green=13;blue=14} -> Some g.id
      | _ -> None
    )
    |> Seq.fold_left (fun sum id -> id + sum) 0

