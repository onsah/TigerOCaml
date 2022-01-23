type label = Temp.label [@@deriving show]

type size [@@deriving show]

type stmt =
  | Seq of stmt list
  | Label of label
  | Jump of
      { expr : expr
      ; labels : label list
      }
  | CondJump of
      { cond : relop
      ; left_expr : expr
      ; right_expr : expr
      ; false_label : label
      ; true_label : label
      }
  | Move of
      { location : expr
      ; value : expr
      }
  (* Discard return result *)
  | Expr of expr
[@@deriving show]

and expr =
  | Binop of
      { left : expr
      ; right : expr
      ; op : binop
      }
  | Mem of expr
  | Temp of Temp.temp
  | ESeq of stmt * expr
  | Name of label
  | Const of int
  | Call of
      { func : expr
      ; args : expr list
      }
[@@deriving show]

and binop =
  | Plus
  | Minus
  | Mul
  | Div
  | And
  | Or
  | Lshift
  | Rshift
  | ArSshift
  | Xor
[@@deriving show]

and relop =
  | Eq
  | Ne
  | Lt
  | Gt
  | Le
  | Ge
  | Ult
  | Ule
  | Ugt
  | Uge
[@@deriving show]

val const_true : expr

val const_false : expr

val jump_single_label : label -> stmt

val is_int_and_truthy : expr -> bool

val is_int_and_falsy : expr -> bool

val is_truthy : int -> bool

val is_falsy : int -> bool

module BuiltIns : sig
  val string_equal : Symbol.symbol
end