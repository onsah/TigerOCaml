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
let rec pp_ty ppf ty = pp_ty_checked ~first:true ppf ty ty

(* first is just to prevent first check *)
and pp_ty_checked ?(first = false) ppf check_ty ty =
  match (not first) && check_ty == ty with
  | true ->
      Format.fprintf ppf "<recursive>"
  | false ->
    ( match ty with
    | Int ->
        Format.fprintf ppf "int"
    | String ->
        Format.fprintf ppf "string"
    | Record (fields, _) ->
        Format.fprintf ppf "{ " ;
        Format.pp_print_list
          ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
          (fun ppf field -> pp_field_checked ppf check_ty field)
          ppf
          fields ;
        Format.fprintf ppf " }"
    | Array (item_ty, _) ->
        Format.fprintf ppf "array[%s]" (show_ty item_ty)
    | Nil ->
        Format.fprintf ppf "nil"
    | Unit ->
        Format.fprintf ppf "()"
    | Name (_, ty_opt) ->
      ( match ty_opt.contents with
      | Some ty ->
          pp_ty_checked ppf check_ty ty
      | None ->
          Format.fprintf ppf "<unbound-name>" ) )


and show_ty ty =
  let _ = pp_ty Format.str_formatter ty in
  Format.flush_str_formatter ()


and pp_field_checked ppf check_ty field =
  let { field_id; field_ty } = field in
  match check_ty == field_ty with
  | true ->
      Format.fprintf ppf "<recursive>"
  | false ->
      Symbol.pp_symbol ppf field_id ;
      Format.fprintf ppf ": " ;
      pp_ty_checked ppf check_ty field_ty


and pp_field ppf = function
  | { field_id; field_ty } ->
      Format.fprintf
        ppf
        "%s: %s"
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
