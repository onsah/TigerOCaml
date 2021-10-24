type label = Temp.label [@@deriving show]

type size = int [@@deriving show]

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

let const_true = Const 1

let const_false = Const 0

(*Convenience function to jump to a single label*)
let jump_single_label label = Jump { expr = Name label; labels = [ label ] }
