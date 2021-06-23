
module Tokens = struct
  
  let string (text, line, col) = String.concat "" ["STRING("; text; "): "; string_of_int line; ","; string_of_int col ];;
  let integer (value, line, col) = "INTEGER(" ^ (string_of_int value) ^ "): " ^ (string_of_int line) ^ ", " ^ (string_of_int col);;
  let identifier (name, line, col) = "IDENTIFIER(" ^ name ^ "): " ^ (string_of_int line) ^ ", " ^ (string_of_int col);;

  (*Start keywords*)
let whileKeyword (line, col) = (String.concat ("")) [ "While: "; string_of_int line ; ","; string_of_int col ] ;;
let forKeyword (line, col) = (String.concat ("")) [ "For: "; string_of_int line ; ","; string_of_int col ] ;;
let toKeyword (line, col) = (String.concat ("")) [ "To: "; string_of_int line ; ","; string_of_int col ] ;;
let breakKeyword (line, col) = (String.concat ("")) [ "Break: "; string_of_int line ; ","; string_of_int col ] ;;
let letKeyword (line, col) = (String.concat ("")) [ "Let: "; string_of_int line ; ","; string_of_int col ] ;;
let inKeyword (line, col) = (String.concat ("")) [ "In: "; string_of_int line ; ","; string_of_int col ] ;;
let endKeyword (line, col) = (String.concat ("")) [ "End: "; string_of_int line ; ","; string_of_int col ] ;;
let functionKeyword (line, col) = (String.concat ("")) [ "Function: "; string_of_int line ; ","; string_of_int col ] ;;
let varKeyword (line, col) = (String.concat ("")) [ "Var: "; string_of_int line ; ","; string_of_int col ] ;;
let typeKeyword (line, col) = (String.concat ("")) [ "Type: "; string_of_int line ; ","; string_of_int col ] ;;
let arrayKeyword (line, col) = (String.concat ("")) [ "Array: "; string_of_int line ; ","; string_of_int col ] ;;
let ifKeyword (line, col) = (String.concat ("")) [ "If: "; string_of_int line ; ","; string_of_int col ] ;;
let thenKeyword (line, col) = (String.concat ("")) [ "Then: "; string_of_int line ; ","; string_of_int col ] ;;
let elseKeyword (line, col) = (String.concat ("")) [ "Else: "; string_of_int line ; ","; string_of_int col ] ;;
let doKeyword (line, col) = (String.concat ("")) [ "Do: "; string_of_int line ; ","; string_of_int col ] ;;
let ofKeyword (line, col) = (String.concat ("")) [ "Of: "; string_of_int line ; ","; string_of_int col ] ;;
let nilKeyword (line, col) = (String.concat ("")) [ "Nil: "; string_of_int line ; ","; string_of_int col ] ;;
(*End keywords*)

  (*Start symbols*)
let commaSymbol (line, col) = (String.concat ("")) [ "COMMA: "; string_of_int line ; ","; string_of_int col ] ;;
let colonSymbol (line, col) = (String.concat ("")) [ "COLON: "; string_of_int line ; ","; string_of_int col ] ;;
let semicolonSymbol (line, col) = (String.concat ("")) [ "SEMICOLON: "; string_of_int line ; ","; string_of_int col ] ;;
let lparenSymbol (line, col) = (String.concat ("")) [ "LPAREN: "; string_of_int line ; ","; string_of_int col ] ;;
let rparenSymbol (line, col) = (String.concat ("")) [ "RPAREN: "; string_of_int line ; ","; string_of_int col ] ;;
let lbrackSymbol (line, col) = (String.concat ("")) [ "LBRACK: "; string_of_int line ; ","; string_of_int col ] ;;
let rbrackSymbol (line, col) = (String.concat ("")) [ "RBRACK: "; string_of_int line ; ","; string_of_int col ] ;;
let lcurlySymbol (line, col) = (String.concat ("")) [ "LCURLY: "; string_of_int line ; ","; string_of_int col ] ;;
let rcurlySymbol (line, col) = (String.concat ("")) [ "RCURLY: "; string_of_int line ; ","; string_of_int col ] ;;
let dotSymbol (line, col) = (String.concat ("")) [ "DOT: "; string_of_int line ; ","; string_of_int col ] ;;
let plusSymbol (line, col) = (String.concat ("")) [ "PLUS: "; string_of_int line ; ","; string_of_int col ] ;;
let minusSymbol (line, col) = (String.concat ("")) [ "MINUS: "; string_of_int line ; ","; string_of_int col ] ;;
let timesSymbol (line, col) = (String.concat ("")) [ "TIMES: "; string_of_int line ; ","; string_of_int col ] ;;
let divSymbol (line, col) = (String.concat ("")) [ "DIV: "; string_of_int line ; ","; string_of_int col ] ;;
let eqSymbol (line, col) = (String.concat ("")) [ "EQ: "; string_of_int line ; ","; string_of_int col ] ;;
let ltgtSymbol (line, col) = (String.concat ("")) [ "LTGT: "; string_of_int line ; ","; string_of_int col ] ;;
let ltSymbol (line, col) = (String.concat ("")) [ "LT: "; string_of_int line ; ","; string_of_int col ] ;;
let lteqSymbol (line, col) = (String.concat ("")) [ "LTEQ: "; string_of_int line ; ","; string_of_int col ] ;;
let gtSymbol (line, col) = (String.concat ("")) [ "GT: "; string_of_int line ; ","; string_of_int col ] ;;
let gteqSymbol (line, col) = (String.concat ("")) [ "GTEQ: "; string_of_int line ; ","; string_of_int col ] ;;
let andSymbol (line, col) = (String.concat ("")) [ "AND: "; string_of_int line ; ","; string_of_int col ] ;;
let orSymbol (line, col) = (String.concat ("")) [ "OR: "; string_of_int line ; ","; string_of_int col ] ;;
let assignSymbol (line, col) = (String.concat ("")) [ "ASSIGN: "; string_of_int line ; ","; string_of_int col ] ;;
(*End symbols*)



end