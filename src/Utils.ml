(* This is used for placeholder for an implementation *)
let __ _ = raise (Failure "Not implemented")

(** Check if a list has duplicate element *)
let rec has_duplicates list =
  match list with
  | [] ->
      false
  | hd :: tl ->
      List.exists (( = ) hd) tl || has_duplicates tl


let rec find_duplicate_opt list =
  match list with
  | [] ->
      None
  | hd :: tl ->
    ( match List.find_opt (( = ) hd) tl with
    | Some name ->
        Some name
    | None ->
        find_duplicate_opt tl )


module ListUtils = struct
  let last list =
    let rec last_impl = function
      | [] ->
          None
      | [ x ] ->
          Some x
      | _ :: xs ->
          last_impl xs
    in
    last_impl list


  let last_unsafe list =
    match last list with Some i -> i | None -> failwith "last_unsafe empty"


  let partition_last list =
    let rec partition_last_impl passed = function
      | [] ->
          failwith "partition_last empty"
      | [ x ] ->
          (List.rev passed, x)
      | x :: xs ->
          partition_last_impl (x :: passed) xs
    in
    partition_last_impl [] list
end
