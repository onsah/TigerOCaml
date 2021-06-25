type unary_op =
| UnaryMinus
[@@deriving show]

type expr =
  | StringExpr of string
  | IntExpr of int
  | NilExpr
  | UnaryExpr of unary_op * expr
  | Temp (* TODO: remove this *)
[@@deriving show]