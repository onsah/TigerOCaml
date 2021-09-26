type translateDebug = 
  | VarDebug of Translate.access
  | FunDebug of Translate.level * Temp.label 
and expr =
  { translated_expr : Translate.expr
  ; pos : Syntax.pos
  ; debug: translateDebug option (* temporary field for testing *)
  }
[@@deriving show]

type typedExpr =
  { translated_expr : expr
  ; ty : Types.ty
  }
[@@deriving show]
