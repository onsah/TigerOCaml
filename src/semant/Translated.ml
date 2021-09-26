type expr =
  { translated_expr : Translate.expr
  ; pos : Syntax.pos
  }
[@@deriving show]

type typedExpr =
  { translated_expr : expr
  ; ty : Types.ty
  }
[@@deriving show]
