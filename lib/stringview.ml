open Util
(* StringView module *)
module Sv = struct
  type t = {
    content: string;
    start: int;
    len: int;
  }

  let of_string content =
    {
      content;
      start = 0;
      len = String.length content;
    }

  let of_substring content start len = 
    { content; start; len; }

  let sub sv start len = 
    if len < 0 then failwith "negative length" else
    if start < sv.start || start >= (sv.start + sv.len) then failwith "invalid start argument" else
    if (start + len) > (sv.start + sv.len) then failwith "invalid len" else
    { sv with start = sv.start + start; len; }

  let copy sv =
    String.sub sv.content sv.start sv.len

  let get i sv = 
    if i < 0 || i >= sv.len then failwith "Out of bounds" else
    sv.content.[sv.start + i]

  let get_unsafe i sv =
    sv.content.[sv.start + i]

  let starts_with (prefix: string) sv : bool =
    let str_len = String.length prefix in
    let rec aux i =
      if i >= str_len then true else
      if i >= sv.len then false else
      if (get i sv) <> (String.get prefix i) then false else
      aux @@ i + 1
    in aux 0

  let starts_with_sv (prefix: t) sv : bool =
    let rec aux i =
      if i >= prefix.len then true else
      if i >= sv.len then false else
      if (get i sv ) <> (get i prefix) then false else
      aux @@ i + 1
    in aux 0

  let eq sv other =
    sv.len = other.len && starts_with_sv sv other

  let to_seq sv =
    let opt i =
      if i >= sv.len then None else
      Some (sv.content.[sv.start + i], i + 1)
    in
    Seq.unfold opt 0

  let chop i sv: (t * t) =
    if i < 0 || i > sv.len then failwith "Out of bounds" else
    ({sv with len = i;}, {sv with start = sv.start + i; len = sv.len - i})

  let expect_string (pattern: string) sv =
    let pattern_len = String.length pattern in
    if starts_with pattern sv then
      Some (chop pattern_len sv) 
    else
      None

  let expect_sv (pattern: t) sv =
    if starts_with_sv sv pattern then
      Some (chop pattern.len sv) 
    else
      None

  let parse_until (f: char -> bool) sv : (t * t) option =
    if sv.len = 0 then None else
    let rec aux i =
      if i >= sv.len then None else
      if get i sv |> f then return @@ chop i sv
      else aux @@ i + 1
    in aux 0
    
  let parse_while (f: char -> bool) sv : (t * t) option =
    if sv.len = 0 then None else
    let rec aux i =
      match i with
      | i when i = 0 && not (f (get i sv)) -> None
      | i when i >= sv.len -> return @@ chop i sv
      | i when not (f (get i sv)) -> return @@ chop i sv
      | i -> aux @@ i + 1
    in aux 0

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

  let contains c sv =
    let is_c x = x = c in
    None <> parse_until is_c sv

  (* keeps consuming character by character until parser returns Some y or the parser runs out of characters *)
  let parse_first (parser: t -> ('a * t) option) sv =
    let rec aux sv' =
      if sv'.len <= 0 then None else
      match parser sv' with
      | None -> aux {sv' with start=sv'.start + 1; len=sv'.len - 1}
      | x -> x
    in aux sv

  let peek_left sv =
    get_unsafe ~-1 sv

  let peek_right sv =
    get_unsafe sv.len sv

  (* let split_on_delim sv (delim: string) = *)
    (* let opt rest = *)
    (*   if rest.len = 0 then None else *)
    (*   match expect_string delim sv with *)
    (*   | Some (_, _) -> None *)
    (* in *)
    (* failwith "unimplemented" *)
  (* keeps parsing until f returns true *)
  (* let peek_left =  *)
  (**)
  module Infix = struct
    let ( >:> ) (x: (t * t) option) (f: t -> 'a) : 'a =
      match x with
      | None -> failwith "cannot unbox None"
      | Some (x, _) -> f x
      
    let ( >~> ) (x: ('a * t) option) (f: t -> ('b * t) option) : ('b * t) option =
      match x with
      | None -> None
      | Some (_, rest) -> f rest

    let ( <~< ) (x: ('a * t) option) (f: t -> ('b * t) option) : ('a * t) option =
      match x with
      | None -> None
      | Some (a, rest) -> 
        let* (_, rest) = f rest in
        return (a, rest)

    let ( >>/ ) x f =
      of_string x |> f
  end

end
