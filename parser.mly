%{ 
    open TigerTokens
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

%start <Syntax.expr> program

%%

let program := 
    ~ = expr; EOF; <>

let expr :=
  | unary_expr
  | literal_expr

let unary_expr :=
  | unary_op; expr

let unary_op :=
  | MINUS

let literal_expr := 
  | stringLit(
      | ~ = STRING; <>
  )
  | intLit(
      | ~ = INT; <>
  )  

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

