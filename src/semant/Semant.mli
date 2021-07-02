type valueEnv = Env.envEntry Symbol.table
type typeEnv = Env.ty Symbol.table

val type_check: Syntax.expr -> Translated.typedExpr