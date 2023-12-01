let int_of_char' input = match input with
  | '0'..'9' -> (int_of_char input) - (int_of_char '0')
  | _ -> -1

let number_int_prefixes: int Util.prefix_assoc list = [("one", 1);("two", 2); ("three", 3 ); ("four", 4);
  ("five", 5); ("six", 6); ("seven", 7); ("eight", 8); ("nine", 9)];;

(* takes in a string, and either returns some of the parsed number (name or digit) or none *)
(* string must start with it *)
let parse_prefix (input: string) : int option = 
  (* let () = input |> Printf.printf "%s\n" in *)
  let len = String.length input in
  if len = 0 then None else
  let res = match input.[0] with
  | '0'..'9' -> Some (int_of_char' input.[0])
  | _ -> Util.find_prefix input number_int_prefixes
  in res

let parse_suffix (input: string) : int option =
  (* let () = input |> Printf.printf "%s\n" in *)
  let len = String.length input in
  if len = 0 then None else
  let res = match input.[len-1] with
  | '0'..'9' -> Some (int_of_char' input.[len-1])
  | _ -> Util.find_suffix input number_int_prefixes
  in res
  

let get_calibration_value (line: string): int =
  (* tens place: *)
  let tens = match Util.prefix_find_map parse_prefix line with 
    | None -> 0
    | Some x -> x
  in
  let ones = match Util.suffix_find_map parse_suffix line with 
    | None -> 0
    | Some x -> x
  in
  let ans = tens * 10 + ones in
  Printf.printf "%s -> %d\n" line ans;
  ans

let part_one infile = 
  let seq = Util.seq_of_filename infile in
  seq |> Seq.fold_left (fun acc line -> acc + get_calibration_value line) 0

