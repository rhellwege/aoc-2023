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

  let to_seq sv =
    let opt i =
      if i >= sv.len then None else
      Some (sv.content.[sv.start + i], i + 1)
    in
    Seq.unfold opt 0

  let chop sv i : (t * t) =
    if i < 0 || i >= sv.len then failwith "Out of bounds" else
    ({sv with len = i;}, {sv with start = sv.start + i; len = sv.len - i})

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
end
