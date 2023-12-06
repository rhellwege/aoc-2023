open Util


(* StringView module *)
module Sv = struct
  type t = {
    content: string;
    start: int;
    len: int;
  }
  type 'a parsed = ('a * t) option
  type almost_parsed = t parsed
  type 'a parser = (t -> 'a parsed)

  let of_string content =
    {
      content;
      start = 0;
      len = String.length content;
    }

  let of_substring content start len = 
    { content; start; len; }

  let sub sv start len = 
    if len < 0 then None else
    if start < sv.start || start >= (sv.start + sv.len) then None else
    if (start + len) > (sv.start + sv.len) then None else
    Some { sv with start = sv.start + start; len; }

  let copy sv =
    String.sub sv.content sv.start sv.len

  let get i sv = 
    if i < 0 || i >= sv.len then None else
    Some sv.content.[sv.start + i]

  let clamp sv i =
    Int.min (Int.max 0 i) (sv.len - 1)

  let get_unsafe i sv =
    let str_len = String.length sv.content in
    if sv.start + i < 0 || sv.start + i >= str_len then None else
    return sv.content.[sv.start + i]

  let starts_with (prefix: string) sv : bool =
    let str_len = String.length prefix in
    let rec aux i =
      if i >= str_len then true else
      if i >= sv.len then false else
      if (get_opt @@ get i sv) <> (String.get prefix i) then false else
      aux @@ i + 1
    in aux 0

  let starts_with_sv (prefix: t) sv : bool =
    let rec aux i =
      if i >= prefix.len then true else
      if i >= sv.len then false else
      if (get_opt @@ get i sv ) <> (get_opt @@ get i prefix) then false else
      aux @@ i + 1
    in aux 0

  let ends_with (suffix: string) sv : bool =
    let str_len = String.length suffix in
    let rec aux i =
      if i < 0 then true else
      if (get_opt @@ get i sv) <> (String.get suffix i) then false else
      aux @@ i - 1
    in aux @@ str_len - 1

  let ends_with_sv (suffix: t) sv : bool =
    let str_len = suffix.len in
    let rec aux i =
      if i < 0 then true else
      if (get_opt @@ get i sv) <> (get_opt @@ get i suffix) then false else
      aux @@ i - 1
    in aux @@ str_len - 1

  let eq sv other =
    sv.len = other.len && starts_with_sv sv other

  let to_seq sv =
    let opt i =
      if i >= sv.len then None else
      Some (sv.content.[sv.start + i], i + 1)
    in
    Seq.unfold opt 0

  let chop i sv: (t * t) option =
    if i < 0 || i > sv.len then None else
    Some ({sv with len = i;}, {sv with start = sv.start + i; len = sv.len - i})

  let expect_string (pattern: string) sv =
    let pattern_len = String.length pattern in
    if starts_with pattern sv then
      chop pattern_len sv
    else
      None

  let expect_sv (pattern: t) sv =
    if starts_with_sv sv pattern then
      chop pattern.len sv
    else
      None

  (* TODO: make this accept a parser to be more composable *)
  let parse_until (f: char -> bool) sv : (t * t) option =
    if sv.len = 0 then None else
    let rec aux i =
      if i >= sv.len then None else
      if get i sv |> get_opt |> f then chop i sv
      else aux @@ i + 1
    in aux 0
    
  (* TODO: make this accept a parser to be more composable *)
  let parse_while (f: char -> bool) sv : (t * t) option =
    if sv.len = 0 then None else
    let rec aux i =
      match i with
      | i when i = 0 && not (f (get i sv |> get_opt)) -> None
      | i when i >= sv.len -> chop i sv
      | i when not (f (get i sv |> get_opt)) -> chop i sv
      | i -> aux @@ i + 1
    in aux 0

  (* goes before t.start *)
  let grow_while_left (f: char -> bool) sv =
    let rec aux i =
      if ~=(get_unsafe i sv >>>= f) then aux (i-1) else
      let len = ~-i + sv.len - 1 in
      let start = Int.max 0 (sv.start+i) in
        return ({sv with start=start; len=len}, {sv with start=start+len; len=String.length sv.content - (start + len)})
    in aux ~-1
  (* breaks out of the confines of sv to search for the largest possible sequence *)
  (* example: 12-3-45 -> -12345- *)
  (* will never shrink, will only grow *)
  let grow_while (f: char -> bool) sv =
    if sv.len = 0 then None else
    let rec aux i =
      match i with
      | i when i = 0 && not (f (get i sv |> get_opt)) -> None
      | i when i >= sv.len -> chop i sv
      | i when not (f (get i sv |> get_opt)) -> chop i sv
      | i -> aux @@ i + 1
    in aux 0

  let parse_char sv = 
    let* (l, r) = chop 1 sv in
    return (l.content.[l.start], r)

  let parse_word sv = 
    let is_alpha = function
    | 'A'..'Z' | 'a'..'z' -> true
    | _ -> false in
    parse_while is_alpha sv

  let parse_word_special sv = 
    let is_alpha = function
    | ' ' | '\t' | '\r' -> false
    | _ -> true in
    parse_while is_alpha sv

  let parse_whitespace sv = 
    let is_whitespace = function
    | ' ' | '\t' | '\r' -> true
    | _ -> false in
    parse_while is_whitespace sv

  let parse_int_sv sv =
    let is_digit = function
    | '0'..'9' -> true
    | _ -> false in
    parse_while is_digit sv

  let parse_int sv =
    let* (l, r) = parse_int_sv sv in
    return (int_of_string (copy l), r)

  let split_on_whitespace sv =
    let opt s =
      if s.len = 0 then None else
      match parse_word_special s with
      | None -> (match parse_whitespace s with
        | None -> None
        | Some (_, remaining) -> parse_word_special remaining
        )
      | x -> x
    in
    sv |> Seq.unfold opt

  let contains (f: char -> bool) sv =
    None <> parse_until f sv

  (* keeps consuming character by character until parser returns Some y or the parser runs out of characters *)
  let parse_first (p: 'a parser) sv =
    let rec aux sv' =
      if sv'.len <= 0 then None else
      match p sv' with
      | None -> aux {sv' with start=sv'.start + 1; len=sv'.len - 1}
      | x -> x
    in aux sv

  (* keeps consuming character by character until parser returns Some y or the parser runs out of characters *)
  let parse_last (p: 'a parser) sv =
    let rec aux sv' =
      if sv'.len <= 0 then None else
      (* print_endline @@ copy sv'; *)
      match parse_first p sv' with
      | None -> None
      | Some (x, rest) -> (
        match parse_first p rest with
        | None -> return (x, rest)
        | Some _ -> aux rest
        )
    in aux sv

  (* parse everything up to the parser pattern, then silently consume the parsed content *)
  let parse_except (p: t parser) sv =
    if sv.len = 0 then None else
    match parse_first p sv with
    | None -> return (sv, {sv with start=sv.start+sv.len; len=0})
    | Some (l, r) -> 
      return ({sv with len=l.start-sv.start}, r)

  let peek_left sv =
    get_unsafe ~-1 sv

  let peek_right sv =
    get_unsafe sv.len sv

  let parse_sequence (p: 'a parser) sv =
    let opt sv' =
      match parse_first p sv' with
      | None -> None
      | Some (a, rest) -> Some (a, rest)
    in
    sv |> Seq.unfold opt

  let split_with (p: 'a parser) sv =
    let opt sv' =
      match  (parse_except p) sv' with
      | None -> None
      | Some (a, rest) -> 
        if a.len = 0 then (parse_except p) rest else
        Some (a, rest)
    in
    sv |> Seq.unfold opt

  let split_on_delim (delim: string) sv =
    split_with (expect_string delim) sv

  (* returns the first succesful parse, if all fail, return none *)
  let parse_either (pl: 'a parser list) sv =
    pl |> List.find_map (fun f -> f sv)

  let parse_and (f: t parser) (g: t parser) : t parser =
    (fun x -> 
      let* (l0, r0) = f x in
      let* (_, _) = g l0 in
      return (l0, r0)
    )

  let parse_combine (f: t parser) (g: t parser) : t parser =
    (fun x -> 
      let* (l0, r0) = f x in
      let* (l1, r1) = g r0 in
      return ({l0 with len=l0.len+l1.len}, r1)
    )

  let parse_discard_left (x: 'a parsed) (f: 'b parser) : 'b parsed =
    match x with
    | None -> None
    | Some (_, rest) -> f rest

  let parse_discard_right (x: 'a parsed) (f: 'b parser) : 'a parsed =
    match x with
    | None -> None
    | Some (a, rest) -> 
      let* (_, rest) = f rest in
      return (a, rest)

  module Infix = struct
    (* if x is a t * t parser result, pass the parsed result to the next parser *)
    let ( >:> ) (x: (t * t) option) (f: t -> 'a) : 'a option =
      match x with
      | None -> None
      | Some (x, _) -> return @@ f x
      
    (* discard the value of x and apply the rest to f and return the result of f *)
    let ( >~> ) (x: 'a parsed) (f: 'b parser) : 'b parsed =
      parse_discard_left x f

    (* given a parser result, apply the rest with f and return the result of x *)
    let ( <~< ) (x: 'a parsed) (f: 'b parser) : 'a parsed =
      parse_discard_right x f

    (* combines the results of two parsers, they must be almost parsers *)
    let ( >+> ) (f: t parser) (g: t parser) : t parser =
      parse_combine f g

    (* applies the left side of f to g then returns the left of g and the remaining of f *)
    let ( >&> ) (f: t parser) (g: t parser) : t parser =
      parse_and f g

    let ( >|> ) (f: 'a parser) (g: 'b parser) : 'b parser =
      (fun x -> x |> f >~> g)

    let ( <|< ) (f: 'a parser) (g: 'b parser) : 'a parser =
      (fun x -> x |> f <~< g)

    (* convert string to stringview *)
    let ( >>/ ) x f =
      of_string x |> f
  end

end
