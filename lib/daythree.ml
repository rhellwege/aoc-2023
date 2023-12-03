open Stringview
(* open Util *)

let contains_symbol sv =
  let is_symbol = function
    | '0'..'9' | '.' -> false
    | _ -> true
  in
  None <> Sv.parse_until is_symbol sv

(* given the integer stringview, return the same string view expanded on both sides *)
(* let search_sv sv = *)
(*   {sv with  *)
(*     start = Int.max 0 (sv.start - 1);  *)
(*     len = Int.min (sv.start + sv.len + 1) (String.length sv.content - 1)  *)
(*   } *)

let part_one _ =
  1

let process infile = 
  let lines = Util.seq_of_filename infile in
  part_one lines


