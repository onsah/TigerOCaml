type ty = Types.ty

type envEntry =
  | VarEntry of ty
  | FunEntry of {argTypes: ty list; return_type: ty}

let baseTypes = [
	(Symbol.symbol "int", Types.Int); 
	(Symbol.symbol "string", Types.String);
	(Symbol.symbol "unit", Types.Unit)
]

let baseTypeEnv = Symbol.enter_all (Symbol.empty, baseTypes)

let baseValueEnv = Symbol.empty
