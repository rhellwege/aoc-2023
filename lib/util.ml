type 'a prefix_assoc = string * 'a

let input_line_opt chan = 
  try Some (input_line chan, chan)
  with End_of_file -> close_in chan; None

let seq_of_filename filename = 
  filename |> open_in |> Seq.unfold input_line_opt

let string_shrink_right input =
  let len = String.length input in
  let res = String.sub input 1 (len - 1) in
  Printf.printf "SHRINK RIGHT %s -> %s\n" input res;
  res

let string_shrink_left input =
  let len = String.length input in
  let res = String.sub input 0 (len - 1) in
  Printf.printf "SHRINK LEFT %s -> %s\n" input res;
  res

let reverse_string input =
  input |> String.to_seq |> List.of_seq |> List.rev |> List.to_seq |> String.of_seq

(* like fold_left, except it iterates through a string and passes the
   substrings first starting with the whole string, then getting smaller
   towards the ending character 
*)
let rec substrings_fold_left (fn: ('acc -> string -> 'acc)) (acc: 'acc) (input: string) : 'acc =
  let len = String.length input in 
  if len = 0 then acc
  else 
    substrings_fold_left fn (fn acc input) (string_shrink_right input)

let rec find_prefix s prefixes = (* returns first match *)
  match prefixes with
  | [] -> None
  | (prefix, x) :: rest ->
    if String.starts_with ~prefix:prefix s then Some x
    else find_prefix s rest

let rec find_suffix s suffixes = (* returns the last match *)
  match suffixes with
  | [] -> None
  | (suffix, x) :: rest ->
    if String.ends_with ~suffix:suffix s then Some x
    else find_suffix s rest

let filter_prefixes (input: string) (prefixes: 'a prefix_assoc list) : 'a list = 
  let aux acc s =
    match find_prefix s prefixes with
    | None -> acc
    | Some x -> x :: acc
  in
  substrings_fold_left aux [] input |> List.rev

let rec prefix_find_map (mapping: (string -> 'b option)) (input: string) : 'b option = 
  match mapping input with
  | None -> prefix_find_map mapping (string_shrink_right input)
  | x -> x

let rec suffix_find_map (mapping: (string -> 'b option)) (input: string) : 'b option = 
  match mapping input with
  | None -> suffix_find_map mapping (string_shrink_left input)
  | x -> x
