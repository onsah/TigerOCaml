open Ir

type level [@@deriving show]

type access [@@deriving show]

type cond_args =
  { true_label : IRTree.label
  ; false_label : IRTree.label
  }

(* TODO: hide definition when create functions are there*)
type expr =
  | Expr of IRTree.expr
  | NoValue of IRTree.stmt
  | Cond of (cond_args -> IRTree.stmt)
[@@deriving show]

(* TODO: delete it *)
val dummy_expr : expr

val outermost : level

val new_level :
  parent:level -> name:Temp.label -> formals_escape:bool list -> level

val formals : level -> access list

val name : level -> Temp.label

val alloc_local : level -> bool -> access

val extract_expr : expr -> IRTree.expr

val extract_no_result : expr -> IRTree.stmt

val extract_cond : expr -> cond_args -> IRTree.stmt

val simple_var : access * level -> expr

(* array access, access level, offset access *)
val subscript : access * level * expr -> expr

(* cond, then body, else body *)
val if_else : expr * expr * expr -> expr

val int : int -> expr

val comparison : expr * Syntax.binary_op * expr -> expr
