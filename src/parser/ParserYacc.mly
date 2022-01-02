%{
    open Syntax
    open Symbol
    open TigerError

    type lvalue_follow_t =
      | FieldLvalueFollow of symbol * pos
      | SubscriptLvalueFollow of expr * pos
%}

%token <TigerTokens.stringToken> STRING
%token <TigerTokens.intToken> INT
%token <TigerTokens.identToken> IDENT
(*Start symbol tokens*)
%token COMMA
%token COLON
%token SEMICOLON
%token <Syntax.pos> LPAREN
%token RPAREN
%token <Syntax.pos> LBRACK
%token RBRACK
%token <Syntax.pos> LCURLY
%token RCURLY
%token <Syntax.pos> DOT
%token <Syntax.pos> PLUS
%token <Syntax.pos> MINUS
%token <Syntax.pos> TIMES
%token <Syntax.pos> DIV
%token <Syntax.pos> EQ
%token <Syntax.pos> LTGT
%token <Syntax.pos> LT
%token <Syntax.pos> LTEQ
%token <Syntax.pos> GT
%token <Syntax.pos> GTEQ
%token <Syntax.pos> AND
%token <Syntax.pos> OR
%token <Syntax.pos> ASSIGN
(*Start keyword tokens*)
%token <Syntax.pos> WHILE
%token <Syntax.pos> FOR
%token TO
%token <Syntax.pos> BREAK
%token <Syntax.pos> LET
%token <Syntax.pos> IN
%token END
%token FUNCTION
%token  <Syntax.pos> VAR
%token <Syntax.pos> TYPE
%token <Syntax.pos> ARRAY
%token <Syntax.pos> IF
%token THEN
%token ELSE
%token DO
%token OF
%token <Syntax.pos> NIL
(*End tokens*)
%token EOF

(* These makes these keywords less associative than operators therefore fixes conflicts *)
%nonassoc OF
%nonassoc THEN
%nonassoc ELSE DO
%nonassoc ASSIGN
%left OR
%left AND
%nonassoc EQ LTGT LT GT LTEQ GTEQ 
%left PLUS MINUS
%left TIMES DIV

%start <Syntax.expr> program

%%

let program := 
    ~ = expr; EOF; <>

let expr :=
  | binary_expr
  | assign_expr
  | literal_expr
  | unary_expr
  | paren_expr
  | if_then_else_expr
  | if_then_expr
  | while_expr
  | lvalue_expr
  | let_expr
  | call_expr
  | for_expr
  | break_expr

let assign_expr := 
    ~ = lvalue_expr; pos = ASSIGN; ~ = expr; 
        { 
            match lvalue_expr with 
                | Expr { expr = LValueExpr { lvalue; _ }; _ } -> 
                    let expr = AssignExpr { var = lvalue; expr } in
                        Expr { expr; pos }  
                | _ -> TigerError.unreachable()
        }

let break_expr := pos = BREAK; { Expr { expr = BreakExpr; pos } }

let for_expr := 
    pos = FOR; ident = IDENT; ASSIGN; from = expr; TO; too = expr; DO; body = expr; 
        { 
            let IdentToken (name, _, _) = ident in
            let var = Symbol.symbol name in
            let expr =  ForExpr { var; from; to' = too; body; escape = ref false } in 
                Expr { expr; pos }
        }

let call_expr := ident = IDENT; pos = LPAREN; expr_list = args_expr; RPAREN; {
    let 
      expr = CallExpr { func = Symbol.symbol (TigerTokens.getIdentName(ident)); args = expr_list } 
    in
      Expr { expr; pos }
}

let args_expr := expr_list_opt = option(head = expr; tail = list(COMMA; ~ = expr; <>); { head :: tail }); { 
  match expr_list_opt with 
      | Some list -> list
      | None -> []
}

let let_expr := 
    pos = LET; ~ = decls; body_pos = IN; body_opt = expr_seq?; END; 
        { 
            let body = match body_opt with 
                | Some body -> body 
                | None -> Expr { expr = SeqExpr []; pos = body_pos }
            in
            let expr = LetExpr { decls; body } in
                Expr { expr; pos } 
        }

let decls := decls = list(decl); { decls }

let decl := 
    | type_decls = nonempty_list(type_decl); { TypeDecls type_decls }
    | var_decl
    | fun_decls = nonempty_list(fun_decl); { FunctionDecls fun_decls }

let fun_decl := 
    FUNCTION; ident = IDENT; LPAREN; params = type_fields; RPAREN; ~ = return_type; EQ; body = expr; 
        { 
            let IdentToken (name, line, col) = ident in
            let name = Symbol.symbol name in
            let pos  = { line; col } in
                FunDecl { name; params; return_type; body; pos }
        }

let var_decl := 
    pos = VAR; ident = IDENT; typ = return_type; ASSIGN; value = expr; 
        { 
            let IdentToken (name, _, _) = ident in
            let name = Symbol.symbol name in 
                VarDecl { name; typ; value; pos; escape = ref false }
        }

let return_type := 
    ident_opt = option(COLON; IDENT); 
        { 
            match ident_opt with 
                | Some IdentToken (name, line, col) -> 
                    Some (Type { symbol = Symbol.symbol name; pos = { line; col } })
                | None -> None
        }

let type_decl := 
    pos = TYPE; ident = IDENT; EQ; decl = type_expr; 
        { 
            let IdentToken (name, _, _) = ident in
            let name = Symbol.symbol name in 
                TypeDecl { name; decl; pos }
        }

let type_expr == 
    | ident = IDENT; 
        { 
            let IdentToken (name, line, col) = ident in
            let name = Symbol.symbol name in
            let pos = { line; col } in
                NameDecl { name; pos } 
        }
    | pos = LCURLY; fields = type_fields; RCURLY; 
        { RecordDecl { fields; pos } }
    | pos = ARRAY; OF; ident = IDENT; 
        {  
            let IdentToken (name, _, _) = ident in
            let name = Symbol.symbol name in 
                ArrayDecl { name; pos }
        }   

let type_fields :=
    type_fields_opt = option(head = type_field; tail = list(COMMA; type_field); { head :: tail }); 
        { 
            match type_fields_opt with 
                | Some type_fields -> type_fields
                | None -> []
        }

let type_field := 
    name_id = IDENT; COLON; type_id = IDENT; 
        {  
            let IdentToken (name, line, col) = name_id in
            let name = Symbol.symbol name in 
            let pos = { line; col } in
            let IdentToken (type_name, _, _) = type_id in
            let typ = Symbol.symbol type_name in
                TypedField { name; typ; pos; escape = ref false }
        }

let expr_seq := 
    head = expr; tail = list(SEMICOLON; expr); 
        { 
            let expr = SeqExpr (head :: tail) in 
            let Expr { pos; _ } = head in 
                Expr { expr; pos } 
        }

let lvalue_expr := 
    ident = IDENT; follow_list = list(lvalue_follow); 
        { 
            let IdentToken (name, line, col) = ident in
            let pos = { line; col } in
            let var = SimpleVar { 
                symbol = (Symbol.symbol name); 
                pos
              }
            in
            let follow_fn curr follow = 
                match follow with 
                    | FieldLvalueFollow (symbol, pos) -> 
                        FieldVar { var = curr; symbol; pos }
                    | SubscriptLvalueFollow (expr, pos) ->
                        SubscriptVar { var = curr; expr; pos }
            in
            let lvalue = List.fold_left follow_fn var follow_list in
            let expr = LValueExpr { lvalue } in 
                Expr { expr; pos }                
        }

let lvalue_follow :=
    | pos = DOT; ident = IDENT; {
        let IdentToken (name, _, _) = ident in 
          FieldLvalueFollow (Symbol.symbol name, pos)
    }
    | pos = LBRACK; ~ = expr; RBRACK; { 
        SubscriptLvalueFollow (expr, pos)
    }

let paren_expr := 
    pos = LPAREN; expr_list_opt = option(head = expr; tail = list(SEMICOLON; ~ = expr; <>); { head :: tail }); RPAREN; 
        { 
            let expr_list = match expr_list_opt with
              | Some list -> list 
              | None -> [] 
            in
            let expr = SeqExpr expr_list in
              Expr { expr; pos }
        }

let if_then_else_expr == 
    pos = IF; cond = expr; THEN; then_arm = expr; ELSE; else_arm = expr; 
        { 
          let expr = IfExpr { cond; then_arm; else_arm = Some else_arm } in
                Expr { expr; pos }
        }

let if_then_expr == 
    pos = IF; cond = expr; THEN; then_arm = expr; 
        { 
            let expr = IfExpr { cond; then_arm; else_arm = None } in
                Expr { expr; pos }
        }

let while_expr == 
    pos = WHILE; cond = expr; DO; body = expr; 
        { 
          let expr = WhileExpr { cond; body } in
              Expr { expr; pos }
        }

let binary_expr :=
  | left = expr; ~ = binop; right = expr; {
      match binop with 
        | (BinaryAnd, pos) ->
          let expr = IfExpr { 
              cond = left; 
              then_arm = right; 
              else_arm = Some (Expr { expr = false_expr; pos })
          } in Expr { expr; pos }
        | (BinaryOr, pos) -> 
          let expr = IfExpr { 
              cond = left; 
              then_arm = Expr { expr = true_expr; pos }; 
              else_arm = Some right
          } in Expr { expr; pos }
        | (kind, pos) -> 
          let expr = BinExpr { left; op = kind; right } in
            Expr { expr; pos }
  }

let binop ==
  | pos = PLUS; { (BinaryPlus, pos) }
  | pos = MINUS; { (BinaryMinus, pos) } 
  | pos = TIMES; { (BinaryTimes, pos) } 
  | pos = DIV; { (BinaryDiv, pos) }
  | pos = EQ; { (BinaryEq, pos) } 
  | pos = LTGT; { (BinaryLtgt, pos) } 
  | pos = LT; { (BinaryLt, pos) } 
  | pos = GT; { (BinaryGt, pos) } 
  | pos = LTEQ; { (BinaryLteq, pos) } 
  | pos = GTEQ; { (BinaryGteq, pos) } 
  | pos = AND; { (BinaryAnd, pos) } 
  | pos = OR; { (BinaryOr, pos) } 

let unary_expr :=
  | ~ = unary_op; ~ = expr; {  
      match unary_op with 
        | (kind, pos) -> 
            let dummy_expr = Expr { expr = IntExpr 0; pos } in
            let expr = BinExpr { left = dummy_expr; op = kind; right = expr } in
              Expr { expr; pos }
  }

(*Tiger does not have unary operations in it's AST, so we use binary expr AST*)
let unary_op ==
  | unary_pos = MINUS; { (BinaryMinus, unary_pos) }

let literal_expr == 
  | stringLit(
      | ~ = STRING; <>
  )
  | intLit(
      | ~ = INT; <>
  )  
  | pos = NIL; { Expr { expr = NilExpr; pos } }
  | record_expr
  | array_expr

let record_expr := 
    ident = IDENT; LCURLY; fields = record_body_expr; RCURLY; {  
      let IdentToken (typ, line, col) = ident in
      let expr = RecordExpr { fields; typ = Symbol.symbol typ } in
        Expr { expr; pos = { line; col } }
    }

let record_body_expr := 
  field_list_opt = option(
    head = record_field_expr; 
    tail = list(COMMA; ~ = record_field_expr; <>);
    { head :: tail }
  ); { 
    match field_list_opt with 
      | Some field_list -> field_list
      | None -> []
  }

let record_field_expr := 
  ident = IDENT; pos =  EQ; ~ = expr; { 
    { 
      symbol = Symbol.symbol (TigerTokens.getIdentName(ident));  
      expr;
      pos
    } 
  }

let array_expr == 
    ident = IDENT; LBRACK; size = expr; RBRACK; OF; init_value = expr; 
        { 
            let IdentToken (typ, line, col) = ident in 
            let typ = Symbol.symbol typ in
            let pos = { line; col } in 
            let expr = ArrayExpr { typ; size; init_value } in 
                Expr { expr; pos }
        }

let stringLit(x) :=
    ~ = x; { 
      let StringToken (str, line, col) = x in
        Expr { expr = StringExpr str; pos = { line; col } }
    }

let intLit(x) :=
    ~ = x; { 
      let IntToken (num, line, col) = x in
        Expr { expr = IntExpr num; pos = { line; col } }  
    }

%%

