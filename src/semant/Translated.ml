type expr = { translated_expr: unit; pos: Syntax.pos }
[@@deriving show]

type typedExpr = { translated_expr: expr; ty: Types.ty }
[@@deriving show]