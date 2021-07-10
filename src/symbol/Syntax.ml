open Symbol

type pos = {line: int; col: int} [@@deriving show]

type binary_op =
  | BinaryPlus
  | BinaryMinus
  | BinaryTimes
  | BinaryDiv
  | BinaryEq
  | BinaryLtgt
  | BinaryLt
  | BinaryGt
  | BinaryLteq
  | BinaryGteq
  | BinaryAnd
  | BinaryOr
[@@deriving show]

type expr = Expr of {expr: expr_ast; pos: pos} [@@deriving show]

and field = {symbol: symbol; expr: expr; pos: pos} [@@deriving show]

and var =
  | SimpleVar of {symbol: symbol; pos: pos}
  | FieldVar of {var: var; symbol: symbol; pos: pos}
  | SubscriptVar of {var: var; expr: expr; pos: pos}
[@@deriving show]

and expr_ast =
  | NilExpr
  | IntExpr of int
  | StringExpr of string
  | CallExpr of {func: symbol; args: expr list}
  | BinExpr of {left: expr; op: binary_op; right: expr}
  | RecordExpr of {fields: field list; typ: symbol}
  | SeqExpr of expr list
  | AssignExpr of {var: var; expr: expr}
  | LValueExpr of {lvalue: var}
  | IfExpr of {cond: expr; then_arm: expr; else_arm: expr option}
  | WhileExpr of {cond: expr; body: expr}
  | ForExpr of {var: symbol; escape: bool ref; from: expr; to': expr; body: expr}
  | BreakExpr
  | LetExpr of {decls: decl list; body: expr}
  | ArrayExpr of {typ: symbol; size: expr; init_value: expr}
[@@deriving show]

and decl =
  | FunctionDecls of fundecl list
  | VarDecl of {name: symbol; typ: typ option; value: expr; pos: pos}
  | TypeDecls of typedecl list

and fundecl =
  | FunDecl of
      { name: symbol
      ; params: typed_field list
      ; return_type: typ option
      ; body: expr
      ; pos: pos }

and typ = Type of {symbol: symbol; pos: pos} [@@deriving show]

and typedecl = TypeDecl of {name: symbol; decl: typeexpr; pos: pos}

and typeexpr =
  | NameDecl of {name: symbol; pos: pos}
  | RecordDecl of {fields: typed_field list; pos: pos}
  | ArrayDecl of {name: symbol; pos: pos}

and typed_field =
  | TypedField of {name: symbol; typ: symbol; pos: pos; escape: bool ref}
