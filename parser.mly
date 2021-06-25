%{
    open Syntax
%}

%token <TigerTokens.stringToken> STRING
%token <TigerTokens.intToken> INT
%token <TigerTokens.identToken> IDENT
(*Start symbol tokens*)
%token COMMA
%token COLON
%token SEMICOLON
%token LPAREN
%token RPAREN
%token LBRACK
%token RBRACK
%token LCURLY
%token RCURLY
%token DOT
%token PLUS
%token MINUS
%token TIMES
%token DIV
%token EQ
%token LTGT
%token LT
%token LTEQ
%token GT
%token GTEQ
%token AND
%token OR
%token ASSIGN
(*Start keyword tokens*)
%token WHILE
%token FOR
%token TO
%token BREAK
%token LET
%token IN
%token END
%token FUNCTION
%token VAR
%token TYPE
%token ARRAY
%token IF
%token THEN
%token ELSE
%token DO
%token OF
%token NIL
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

let break_expr := BREAK; { Temp }

let for_expr := FOR; IDENT; ASSIGN; expr; TO; expr; DO; expr; { Temp }

let call_expr := IDENT; LPAREN; args_expr; RPAREN; { Temp }

let args_expr := option(expr; list(COMMA; expr)); { Temp }

let let_expr := LET; decls; IN; expr_seq?; END; { Temp }

let decls := decl*; { Temp }

let decl := 
    | type_decl; { Temp }
    | var_decl; { Temp }
    | fun_decl; { Temp }

let fun_decl := FUNCTION; IDENT; LPAREN; type_fields; RPAREN; return_type; EQ; expr; { Temp }

let var_decl := VAR; IDENT; return_type ; ASSIGN; expr; { Temp }

let return_type := option(COLON; IDENT); { Temp }

let type_decl := TYPE; IDENT; EQ; type_expr; { Temp }

let type_expr == 
    | IDENT; { Temp }
    | LCURLY; type_fields; RCURLY; { Temp }
    | ARRAY; OF; IDENT; { Temp }

let type_fields := option(type_field; list(COMMA; type_field)); { Temp }

let type_field := IDENT; COLON; IDENT; { Temp }

let expr_seq := expr; list(SEMICOLON; expr); { Temp }

(*TODO: this conflicts with array expr. It is safe to leave this way but would be good if we can eliminate*)
let lvalue_expr := IDENT; lvalue_follow*; { Temp }

let lvalue_follow :=
    | DOT; IDENT; { Temp }
    | LBRACK; expr; RBRACK; { Temp }

let paren_expr := LPAREN; expr; list(SEMICOLON; expr); RPAREN; { Temp }

let if_then_else_expr == IF; expr; THEN; expr; ELSE; expr; { Temp }

let if_then_expr == IF; expr; THEN; expr; { Temp }

let while_expr == WHILE; expr; DO; expr; { Temp }

let binary_expr :=
  | expr; binop; expr; { Temp }

let binop ==
  | PLUS | MINUS | TIMES | DIV
  | EQ | LTGT | LT | GT | LTEQ | GTEQ 
  | AND | OR | ASSIGN

let unary_expr :=
  | ~ = unary_op; ~ = expr; { UnaryExpr (unary_op, expr) }

let unary_op ==
  | MINUS; { UnaryMinus }

let literal_expr == 
  | stringLit(
      | ~ = STRING; <>
  )
  | intLit(
      | ~ = INT; <>
  )  
  | NIL; { NilExpr }
  | LPAREN; RPAREN; { Temp }
  | record_expr
  | array_expr

let record_expr := IDENT; LCURLY; record_body_expr?; RCURLY; { Temp }

let record_body_expr := record_field_expr; list(COMMA; record_field_expr); { Temp }

let record_field_expr := IDENT; EQ; expr; { Temp }

let array_expr == IDENT; LBRACK; expr; RBRACK; OF; expr; { Temp }

let stringLit(x) :=
    ~ = x; { 
        match x with 
            | StringToken (str, _, _) -> StringExpr str  
    }

let intLit(x) :=
    ~ = x; { 
        match x with 
            | IntToken (num, _, _) -> IntExpr num  
    }

%%

