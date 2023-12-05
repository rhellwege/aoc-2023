(* MONAD *)
let bind (m: 'a option) (f: 'a -> 'b option) : 'b option =
  match m with
  | None -> None
  | Some x -> f x

let upgrade (w: 'a option) (f: 'a -> 'b) : 'b option =
  match w with
  | None -> None
  | Some x -> Some (f x);;

let ( >>= ) = bind
let ( >>>=  ) = upgrade
let ( let* ) = ( >>= )
let ( let+ ) = ( >>>= )

let ( ~= ) = function
  | None -> false
  | Some x -> x

let return x = Some x
(* let ( >> ) f g x = *)
  (* x |> f |> g *)

type 'a pattern_assoc = string * 'a

let get_opt = function
  | Some x -> x
  | None -> failwith "get_opt failed (got none)"
(* returns a lazy sequence of lines from a filename *)
let seq_of_filename (filename: string) : string Seq.t = 
  let input_line_opt chan = 
    try Some (input_line chan, chan)
    with End_of_file -> close_in chan; None
  in
  filename |> open_in |> Seq.unfold input_line_opt

(* let slurp *)

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
let substrings_fold_left (fn: ('acc -> string -> 'acc)) (acc: 'acc) (input: string) : 'acc =
  seq_of_substrings_left input |> Seq.fold_left fn acc

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

let expect_string pattern input =
  let len = String.length input in
  let pattern_len = String.length pattern in
  if String.starts_with ~prefix:pattern input then
    Some (pattern, String.sub input pattern_len (len - pattern_len)) 
  else
    None

let parse_word input = 
  let len = String.length input in
  if len = 0 then None else
  let is_alpha = function
  | 'A'..'Z' | 'a'..'z' -> true
  | _ -> false in
  let rec aux i = 
    match i with
    | i when i = 0 && not (is_alpha input.[i]) -> None
    | i when i >= len -> Some (String.sub input 0 i, "")
    | i when not (is_alpha input.[i]) -> Some (String.sub input 0 i, String.sub input i (len - i))
    | i -> aux @@ i + 1
  in 
  aux 0

let parse_word_special input = 
  let len = String.length input in
  if len = 0 then None else
  let is_alpha = function
  | ' ' | '\t' | '\r' -> false
  | _ -> true in
  let rec aux i = 
    match i with
    | i when i = 0 && not (is_alpha input.[i]) -> None 
    | i when i >= len -> Some (String.sub input 0 i, "")
    | i when not (is_alpha input.[i]) -> Some (String.sub input 0 i, String.sub input i (len - i))
    | i -> aux @@ i + 1
  in 
  aux 0

let parse_whitespace input =
  let len = String.length input in
  if len = 0 then None else
  let is_whitespace = function
  | ' ' | '\t' | '\r' -> true
  | _ -> false in
  let rec aux i = 
    match i with
    | i when i = 0 && not (is_whitespace input.[i]) -> None 
    | i when i >= len -> Some (String.sub input 0 i, "")
    | i when not (is_whitespace input.[i]) -> Some (String.sub input 0 i, String.sub input i (len - i))
    | i -> aux @@ i + 1
  in 
  aux 0

let parse_int input =
  let len = String.length input in
  if len = 0 then None else
  let is_digit = function
  | '0'..'9' -> true
  | _ -> false in
  let rec aux i = 
    match i with
    | i when i = 0 && not (is_digit input.[i]) -> 
      None
    | i when i >= len -> 
      Some (int_of_string (String.sub input 0 i), "")
    | i when not (is_digit input.[i]) -> 
      Some (int_of_string (String.sub input 0 i), String.sub input i (len - i))
    | i -> aux @@ i + 1
  in 
  aux 0

let parse_char input =
  let len = String.length input in
  if len = 0 then None
  else Some (input.[0], string_shrink_right input) 

let split_on_whitespace input =
  let opt s =
    match parse_word_special s with
    | _ when String.length s = 0 -> None 
    | None -> (match parse_whitespace s with
      | None -> None
      | Some (_, remaining) -> parse_word_special remaining
      )
    | x -> x
  in
  input |> Seq.unfold opt

