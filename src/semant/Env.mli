type ty = Types.ty [@@deriving show]

type envEntry =
  | VarEntry of ty
  | FunEntry of
      { argTypes : ty list
      ; return_type : ty
      }
[@@deriving show]

val baseTypeEnv : ty Symbol.table

val baseValueEnv : envEntry Symbol.table
