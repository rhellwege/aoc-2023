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

  let get sv i = 
    if i < 0 || i >= sv.len then failwith "Out of bounds" else
    sv.content.[sv.start + i]


  let starts_with (prefix: string) sv : bool =
    let str_len = String.length prefix in
    let rec aux i =
      if i >= str_len then true else
      if i >= sv.len then false else
      if (get sv i) <> (String.get prefix i) then false else
      aux @@ i + 1
    in aux 0

  let starts_with_sv (prefix: t) sv : bool =
    let rec aux i =
      if i >= prefix.len then true else
      if i >= sv.len then false else
      if (get sv i) <> (get prefix i) then false else
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

  let chop sv i : (t * t) =
    if i < 0 || i > sv.len then failwith "Out of bounds" else
    ({sv with len = i;}, {sv with start = sv.start + i; len = sv.len - i})

  let expect_string (pattern: string) sv =
    let pattern_len = String.length pattern in
    if starts_with pattern sv then
      Some (chop sv pattern_len) 
    else
      None

  let expect_sv (pattern: t) sv =
    if starts_with_sv sv pattern then
      Some (chop sv pattern.len) 
    else
      None

  let parse_until (f: char -> bool) sv : (t * t) option =
    let rec aux i =
      if i >= sv.len then None else
      if get sv i |> f then return @@ chop sv i
      else aux @@ i + 1
    in
    aux 0

  let parse_word sv = 
    if sv.len = 0 then None else
    let is_alpha = function
    | 'A'..'Z' | 'a'..'z' -> true
    | _ -> false in
    let rec aux i = 
      match i with
      | i when i = 0 && not (is_alpha @@ get sv i) -> None
      | i when i >= sv.len -> return @@ chop sv i 
      | i when not (is_alpha @@ get sv i) -> return @@ chop sv i
      | i -> aux @@ i + 1
    in 
    aux 0

  let parse_word_special sv = 
    if sv.len = 0 then None else
    let is_alpha = function
    | ' ' | '\t' | '\r' -> false
    | _ -> true in
    let rec aux i = 
      match i with
      | i when i = 0 && not (is_alpha @@ get sv i) -> None 
      | i when i >= sv.len -> return @@ chop sv i
      | i when not (is_alpha @@ get sv i) -> return @@ chop sv i
      | i -> aux @@ i + 1
    in 
    aux 0

  let parse_whitespace sv = 
    if sv.len = 0 then None else
    let is_whitespace = function
    | ' ' | '\t' | '\r' -> true
    | _ -> false in
    let rec aux i = 
      match i with
      | i when i = 0 && not (is_whitespace @@ get sv i) -> None 
      | i when i >= sv.len -> return @@ chop sv i
      | i when not (is_whitespace @@ get sv i) -> return @@ chop sv i
      | i -> aux @@ i + 1
    in 
    aux 0

  let parse_int_sv sv =
    if sv.len = 0 then None else
    let is_not_digit = function
    | '0'..'9' -> false
    | _ -> true in
    parse_until is_not_digit sv

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

  let force sv start len =
    let c_len = String.length sv.content in
    if len < 0 || start < 0 || start + len > c_len then failwith "cannot force out of bounds." else
    {sv with start; end }

  (* let split_on_delim sv (delim: string) = *)
    (* let opt rest = *)
    (*   if rest.len = 0 then None else *)
    (*   match expect_string delim sv with *)
    (*   | Some (_, _) -> None *)
    (* in *)
    (* failwith "unimplemented" *)
  (* keeps parsing until f returns true *)

  module Infix = struct
    let ( ~~> ) (x: ('a * 'b) option) (f: 'b -> ('c * 'b) option) : ('c * 'b) option =
      let* (_, b) = x in
      f b
  end

end
