open Stringview
open Util

let is_symbol = function
  | '0'..'9' | '.' -> false
  | _ -> true

let contains_symbol sv =
  None <> Sv.parse_until is_symbol sv

(* given the integer stringview, return the same string view expanded on both sides  *)
let grow_sv (sv: Sv.t) : Sv.t = 
  let start = Int.max 0 (sv.start - 1) in
  let str_len = String.length sv.content in
  let left_extension = Int.min start 1 in
  let right_extension = Int.min (str_len - (sv.start + sv.len)) 1 in
  let new_len = sv.len + left_extension + right_extension in

  { sv with
    start = start;
    len = new_len;
  }

let count_int (top: Sv.t option) (bottom: Sv.t option) (sv: Sv.t) : int option = 
  let expanded = grow_sv sv in
  let grow (x: Sv.t) = 
    let res = {expanded with content = x.content} in
    res
    in
  let success = return @@ int_of_string (Sv.copy sv) in
  let res = top >>>= grow >>>= contains_symbol in
  if ~=res then success else
  let res = bottom >>>= grow >>>= contains_symbol in
  if ~=res then success else
  let left = sv |> Sv.peek_left >>>= is_symbol in
  if ~=left then success else
  let right = sv |> Sv.peek_right >>>= is_symbol in
  if ~=right then success else
  None

let calc_window top bottom middle =
  let ints = Sv.parse_sequence Sv.parse_int_sv middle |> Seq.filter_map (fun x ->
    let* counted = count_int top bottom x in
    return counted
  ) in
  let result = ints |> Seq.fold_left (fun x acc -> x + acc) 0 in
  result

let part_one lines =
  let lines = lines |> Seq.map Sv.of_string in
  let rec aux (sum, top, middle) rest =
    let bottom = Seq.uncons rest in
    match bottom with
    | None -> sum + calc_window top None middle
    | Some (bottom, rest) ->
      let lsum = calc_window top (Some bottom) middle in
      aux (sum + lsum, Some middle, bottom) rest
  in 
  let mr = Seq.uncons lines in
  match mr with
  | None -> 0
  | Some (middle, rest) -> aux (0, None, middle) rest
  
let process infile = 
  let lines = Util.seq_of_filename infile in
  part_one lines

