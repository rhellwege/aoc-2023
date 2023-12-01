let int_of_char' x = 
  match x with
  | '0'..'9' -> (int_of_char x) - (int_of_char '0')
  | _ -> -1

let get_calibration_value (line: string): int =
  let aux acc c =
    match c with
    | '0'..'9' -> acc ^ (String.make 1 c )
    | _ -> acc in
  let li = String.fold_left aux "" line in
  let first = li.[0] in
  let last =  li.[String.length li - 1] in
  let result = (int_of_char' first) * 10 + int_of_char' last in
  result
  
let input_line_opt chan = 
  try Some (input_line chan, chan)
  with End_of_file -> close_in chan; None

let part_one infile = 
  let seq = infile |> open_in |> Seq.unfold input_line_opt in
  Seq.fold_left (fun acc line -> acc + get_calibration_value line) 0 seq

let part_two input = input
