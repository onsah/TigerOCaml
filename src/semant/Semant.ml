open TigerError
open Translated

module S = Syntax

type valueEnv = Env.envEntry Symbol.table
type typeEnv = Env.ty Symbol.table

let expecting_int ty pos =
	match ty with 
	| Types.Int -> ()
	| _ -> TigerError.semant_error ("Expecting int", pos)

let rec transExpr = function 
	| 	(value_env, type_env, S.Expr { 
			expr = S.BinExpr {
				left; right; op = S.BinaryPlus
			}; pos
		}) ->
			let { ty = ty_left; expr = { pos = pos_left; _ } } = transExpr (value_env, type_env, left)
			and { ty = ty_right; expr = { pos = pos_right; _ } } = transExpr (value_env, type_env, right) in
				expecting_int ty_left pos_left;
				expecting_int ty_right pos_right;
				{ expr = { expr = (); pos }; ty = Types.Int }
	|	(_, _, S.Expr {
			expr = S.IntExpr _; pos
		}) -> { expr = { expr = (); pos }; ty = Types.Int }
  	| 	_ -> TigerError.notImplemented ()

let type_check expr = transExpr (Env.baseValueEnv, Env.baseTypeEnv, expr)