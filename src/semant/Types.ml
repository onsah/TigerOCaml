(** All unit ref's are different than other. Used for unqiuely differentiating each type *)
type unique = unit ref [@@deriving show]

type ty =
  | Int
  | String
  | Record of field list * unique
  | Array of ty * unique
  | Nil
  | Name of Symbol.symbol * ty option ref
      (** Name is used as a placeholder for recursive types and forward declarations*)
  | Unit

and field =
  { field_id : Symbol.symbol
  ; field_ty : ty
  }

(* TODO: make it check until a loop occurs *)
let rec pp_ty ppf = function
  | Int ->
      Format.fprintf ppf "int"
  | String ->
      Format.fprintf ppf "string"
  (* TODO *)
  | Record (fields, _) ->
      Format.pp_print_list (fun ppf field -> pp_field ppf field) ppf fields
  | Array (item_ty, _) ->
      Format.fprintf ppf "array[%s]" (show_ty item_ty)
  | Nil ->
      Format.fprintf ppf "nil"
  | Unit ->
      Format.fprintf ppf "()"
  | Name _ ->
      Format.fprintf ppf "<name>"


and show_ty ty =
  let _ = pp_ty Format.str_formatter ty in
  Format.flush_str_formatter ()


and pp_field ppf = function
  | { field_id; field_ty } ->
      Format.fprintf
        ppf
        "{ %s: %s }"
        (Symbol.show_symbol field_id)
        (show_ty field_ty)


and show_field field =
  let _ = pp_field Format.str_formatter field in
  Format.flush_str_formatter ()


(* Returns a type that is not a `Name`. Extracts the underlying type if encounters a `Name` *)
let rec actual_ty = function
  | Name (_, ty_opt) ->
    ( match ty_opt.contents with
    | Some ty ->
        actual_ty ty
    | None ->
        raise (Invalid_argument "Name ty must be Some") )
  | ty ->
      ty


let has_loop = function
  | Name (symbol, ty_opt) ->
      let rec has_loop_impl = function
        | Some (Name (ty_symbol, ty_opt)) ->
            symbol == ty_symbol || has_loop_impl ty_opt.contents
        | _ ->
            false
      in
      has_loop_impl ty_opt.contents
  | _ ->
      false


let is_record ty = match ty with Record _ -> true | _ -> false

let find_field record field_id =
  List.find_opt (fun rec_field -> rec_field.field_id = field_id) record
