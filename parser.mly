%{ 
    open TigerTokens
    open Syntax 
%}

%token <TigerTokens.stringToken> STRING
%token EOF

%start <Syntax.expr> program

%%

let program := 
    ~ = expr; EOF; <>

let expr :=
  | stringLit(
      | ~ = STRING; <>
  )

let stringLit(x) :=
    ~ = x; { 
        match x with 
            | StringToken (str, _, _) -> StringExpr str  
    }

%%