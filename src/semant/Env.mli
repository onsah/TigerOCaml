type ty

type envEntry = 
    | VarEntry of ty
    | FunEntry of { argTypes: ty list; return_type: ty }

val baseTypeEnv: ty Symbol.table
val baseValueEnv: envEntry Symbol.table