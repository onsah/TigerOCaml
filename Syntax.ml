type expr =
  | StringExpr of string
  | IntExpr of int
  | Temp (* TODO: remove this *)
[@@deriving show]
