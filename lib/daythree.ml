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
  let len = Int.min (sv.len + 2) (str_len - (sv.start + sv.len + 2)) in
  Printf.printf "%d %d \\\\\n" sv.start sv.len;
  Printf.printf "%d %d //\n" start len;
  {sv with  
    start = start;
    len = len;
  } 

let count_int (top: Sv.t option) (bottom: Sv.t option) (sv: Sv.t) : int option = 
  print_endline @@ Sv.copy sv;
  let expanded = grow_sv sv in
  print_endline @@ Sv.copy sv;
  let grow (x: Sv.t) = 
    let res = {expanded with content = x.content} in
    print_endline @@ Sv.copy res;
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
    count_int top (return bottom) x
  ) in
    ints |> Seq.fold_left (fun x acc -> x + acc) 0 

let part_one _ =
  (* let _ = Seq.map Sv.of_string lines in *)
  let rec aux acc top bottom middle =
    match (top, bottom, middle) with
    _ -> 
    match middle with
    | None -> acc
    | _ -> aux acc top bottom middle  
  in aux 0 None None None
      (* let* (new_bottom, lines) = match Seq.uncons  *)
      (* aux (acc + calc_window top bottom middle) middle (Seq.uncons lines)  *)
    
  
let process _ = 
  0
  (* let lines = Util.seq_of_filename infile in *)
  (* part_one lines *)

