let int_of_char' input = match input with
  | '0'..'9' -> (int_of_char input) - (int_of_char '0')
  | _ -> -1

let number_int_assoc: int Util.pattern_assoc list = [("one", 1);("two", 2); ("three", 3 ); ("four", 4);
  ("five", 5); ("six", 6); ("seven", 7); ("eight", 8); ("nine", 9)];;

(* takes in a string, and either returns some of the parsed number (name or digit) or none *)
(* string must start with it *)
let parse_prefix (input: string) : int option = 
  let len = String.length input in
  if len = 0 then None else
  match input.[0] with
  | '0'..'9' -> Some (int_of_char' input.[0])
  | _ -> Util.find_prefix number_int_assoc input 

let parse_suffix (input: string) : int option =
  let len = String.length input in
  if len = 0 then None else
  match input.[len-1] with
  | '0'..'9' -> Some (int_of_char' input.[len-1])
  | _ -> Util.find_suffix number_int_assoc input 

let get_calibration_value (line: string): int =
  let tens = match Util.prefix_find_map parse_prefix line with 
    | None -> 0
    | Some x -> x
  in
  let ones = match Util.suffix_find_map parse_suffix line with 
    | None -> 0
    | Some x -> x
  in
  tens * 10 + ones

let process infile = 
  let seq = Util.seq_of_filename infile in
  seq |> Seq.fold_left (fun acc line -> acc + get_calibration_value line) 0

