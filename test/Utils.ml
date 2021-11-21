open Syntax

let dummy_pos : pos = { col = -1; line = -1 }

let int_expr i = Expr { pos = dummy_pos; expr = IntExpr i }

let expr expr = Expr { pos = dummy_pos; expr }

let simple_var name =
  expr
    (LValueExpr
       { lvalue = SimpleVar { pos = dummy_pos; symbol = Symbol.symbol name } }
    )


let decl_var ?(escape = ref false) name =
  VarDecl
    { name = Symbol.symbol name
    ; typ = None
    ; escape
    ; value = int_expr 0
    ; pos = dummy_pos
    }


let unit = expr (SeqExpr [])
