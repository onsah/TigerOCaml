type expr = { expr: unit; pos: Syntax.pos }
[@@deriving show]

type typedExpr = { expr: expr; ty: Types.ty }
[@@deriving show]