type subgame = int * int * int
type game = {
  id: int;
  set: subgame list;
}

let parse_game line =
  let (_, line) = expect_string "Game " line in
let process infile = 
  let lines = Util.seq_of_filename infile in
  let () = lines |> Seq.iter (fun line -> print_endline line;) in
  1
