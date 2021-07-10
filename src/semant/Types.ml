(** All unit ref's are different than other. Used for unqiuely differentiating each type *)
type unique = unit ref [@@deriving show]

type ty =
  | Int
  | String
  | Record of field list * unique
  | Array of ty * unique
  | Nil
  | Name of Symbol.symbol * ty option ref (** Name is used as a placeholder for recursive types and forward declarations*)
  | Unit
[@@deriving show]

and field =
  { field_id : Symbol.symbol
  ; field_ty : ty
  }
[@@deriving show]

let is_record ty = match ty with Record _ -> true | _ -> false

let find_field record field_id =
  List.find_opt (fun rec_field -> rec_field.field_id = field_id) record
