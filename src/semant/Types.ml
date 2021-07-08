(* All unit ref's are different than other. Used for unqiuely differentiating each type *)
type unique = unit ref [@@deriving show]

type ty =
  | Int
  | String
  | Record of field list * unique
  | Array of ty * unique
  | Nil
  | Unit
  (* Used as a placeholder for recursive types and forward declarations*)
  | Name of Symbol.symbol * ty option ref
[@@deriving show]

and field =
  { field_id : Symbol.symbol
  ; field_ty : ty
  }
[@@deriving show]

let find_field record field_id =
  List.find_opt (fun rec_field -> rec_field.field_id = field_id) record
;;