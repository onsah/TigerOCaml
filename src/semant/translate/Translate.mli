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

type typed_expr =
  { expr : expr
  ; ty : Types.ty
  }
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

val string : string -> expr

val record : fields:expr list -> expr

val array : size:int -> init_expr:expr -> expr

val while' : cond:expr -> body:expr -> break_label:Temp.label -> expr

val for' :
     var:expr
  -> from:expr
  -> to':expr
  -> body:expr
  -> break_label:Temp.label
  -> expr

val break' : Temp.label -> expr

val comparison : typed_expr * Syntax.binary_op * typed_expr -> expr

(* Assumes a called function can only be called by it's statically enclosing function. *)
val function_call :
     label:Temp.label
  -> args:expr list
  -> callee_level:level
  -> caller_level:level
  -> expr

val init_variable : access_expr:expr -> expr:expr -> expr

val let' : init_exprs:expr list -> body:expr -> expr

val func_decl : body:expr -> expr