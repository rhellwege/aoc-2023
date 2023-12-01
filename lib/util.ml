type 'a pattern_assoc = string * 'a

(* returns a lazy sequence of lines from a filename *)
let seq_of_filename (filename: string) : string Seq.t = 
  let input_line_opt chan = 
    try Some (input_line chan, chan)
    with End_of_file -> close_in chan; None
  in
  filename |> open_in |> Seq.unfold input_line_opt

(* The direction (left or right) refers to the direction the list is shrinking *)
let string_shrink_right input =
  let len = String.length input in
  String.sub input 1 (len - 1)

let string_shrink_left input =
  let len = String.length input in
  String.sub input 0 (len - 1)

let seq_of_substrings_right (input: string) : string Seq.t =
  let opt x =
    if String.length x = 0 then None
    else Some (x, string_shrink_right x)
  in
  input |> Seq.unfold opt

let seq_of_substrings_left (input: string) : string Seq.t =
  let opt x =
    if String.length x = 0 then None
    else Some (x, string_shrink_left x)
  in
  input |> Seq.unfold opt

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

let rec find_prefix prefixes s = (* returns first match *)
  match prefixes with
  | [] -> None
  | (prefix, x) :: rest ->
    if String.starts_with ~prefix:prefix s then Some x
    else find_prefix rest s

let rec find_suffix suffixes s = (* returns the last match *)
  match suffixes with
  | [] -> None
  | (suffix, x) :: rest ->
    if String.ends_with ~suffix:suffix s then Some x
    else find_suffix rest s

let filter_prefixes (input: string) (prefixes: 'a pattern_assoc list) : 'a list = 
  let aux acc s =
    match find_prefix prefixes s with
    | None -> acc
    | Some x -> x :: acc
  in
  substrings_fold_left aux [] input |> List.rev

(* given a function mapping that takes in a string, this function will return the first occurence of when mapping returns some value *)
(* moving from left to right *)
let rec prefix_find_map (mapping: (string -> 'b option)) (input: string) : 'b option = 
  match mapping input with
  | None -> prefix_find_map mapping (string_shrink_right input)
  | x -> x

(* given a function mapping that takes in a string, this function will return the first occurence of when mapping returns some value *)
(* moving from right to left *)
let rec suffix_find_map (mapping: (string -> 'b option)) (input: string) : 'b option = 
  match mapping input with
  | None -> suffix_find_map mapping (string_shrink_left input)
  | x -> x
