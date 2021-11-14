type ty = Types.ty [@@deriving show]

type envEntry =
  | VarEntry of
      { access : Translate.access
      ; ty : ty
      }
  | FunEntry of
      { level : Translate.level
      ; label : Temp.label
      ; argTypes : ty list
      ; return_type : ty
      }
[@@deriving show]

val baseTypeEnv : ty Symbol.table

val baseValueEnv : envEntry Symbol.table
