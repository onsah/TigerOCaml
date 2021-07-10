(* This is used for placeholder for an implementation *)
let __ _ = raise (Failure "Not implemented")

(** Check if a list has duplicate element *)
let rec has_duplicates list =
  match list with
  | [] ->
      false
  | hd :: tl ->
      List.exists (( = ) hd) tl || has_duplicates tl
