type stringToken = StringToken of string * int * int

type intToken = IntToken of int * int * int

type identToken = IdentToken of string * int * int

let getIdentName identToken =
    match identToken with 
      | IdentToken (name, _, _) -> name

(*Start keyword types*)
type whileToken = WhileToken of int * int

type forToken = ForToken of int * int

type toToken = ToToken of int * int

type breakToken = BreakToken of int * int

type letToken = LetToken of int * int

type inToken = InToken of int * int

type endToken = EndToken of int * int

type functionToken = FunctionToken of int * int

type varToken = VarToken of int * int

type typeToken = TypeToken of int * int

type arrayToken = ArrayToken of int * int

type ifToken = IfToken of int * int

type thenToken = ThenToken of int * int

type elseToken = ElseToken of int * int

type doToken = DoToken of int * int

type ofToken = OfToken of int * int

type nilToken = NilToken of int * int

(*Start symbol types*)
type commaToken = CommaToken of int * int

type colonToken = ColonToken of int * int

type semicolonToken = SemicolonToken of int * int

type lparenToken = LparenToken of int * int

type rparenToken = RparenToken of int * int

type lbrackToken = LbrackToken of int * int

type rbrackToken = RbrackToken of int * int

type lcurlyToken = LcurlyToken of int * int

type rcurlyToken = RcurlyToken of int * int

type dotToken = DotToken of int * int

type plusToken = PlusToken of int * int

type minusToken = MinusToken of int * int

type timesToken = TimesToken of int * int

type divToken = DivToken of int * int

type eqToken = EqToken of int * int

type ltgtToken = LtgtToken of int * int

type ltToken = LtToken of int * int

type lteqToken = LteqToken of int * int

type gtToken = GtToken of int * int

type gteqToken = GteqToken of int * int

type andToken = AndToken of int * int

type orToken = OrToken of int * int

type assignToken = AssignToken of int * int
(*End types*)

let string (text, line, col) = StringToken (text, line, col)

let integer (value, line, col) = IntToken (value, line, col)

let identifier (name, line, col) = IdentToken (name, line, col)

(*Start keyword functions*)
let whileKeyword (line, col) = WhileToken (line, col)

let forKeyword (line, col) = ForToken (line, col)

let toKeyword (line, col) = ToToken (line, col)

let breakKeyword (line, col) = BreakToken (line, col)

let letKeyword (line, col) = LetToken (line, col)

let inKeyword (line, col) = InToken (line, col)

let endKeyword (line, col) = EndToken (line, col)

let functionKeyword (line, col) = FunctionToken (line, col)

let varKeyword (line, col) = VarToken (line, col)

let typeKeyword (line, col) = TypeToken (line, col)

let arrayKeyword (line, col) = ArrayToken (line, col)

let ifKeyword (line, col) = IfToken (line, col)

let thenKeyword (line, col) = ThenToken (line, col)

let elseKeyword (line, col) = ElseToken (line, col)

let doKeyword (line, col) = DoToken (line, col)

let ofKeyword (line, col) = OfToken (line, col)

let nilKeyword (line, col) = NilToken (line, col)
(*End keyword functions*)

(*Start symbol functions*)
let commaSymbol (line, col) = CommaToken (line, col)

let colonSymbol (line, col) = ColonToken (line, col)

let semicolonSymbol (line, col) = SemicolonToken (line, col)

let lparenSymbol (line, col) = LparenToken (line, col)

let rparenSymbol (line, col) = RparenToken (line, col)

let lbrackSymbol (line, col) = LbrackToken (line, col)

let rbrackSymbol (line, col) = RbrackToken (line, col)

let lcurlySymbol (line, col) = LcurlyToken (line, col)

let rcurlySymbol (line, col) = RcurlyToken (line, col)

let dotSymbol (line, col) = DotToken (line, col)

let plusSymbol (line, col) = PlusToken (line, col)

let minusSymbol (line, col) = MinusToken (line, col)

let timesSymbol (line, col) = TimesToken (line, col)

let divSymbol (line, col) = DivToken (line, col)

let eqSymbol (line, col) = EqToken (line, col)

let ltgtSymbol (line, col) = LtgtToken (line, col)

let ltSymbol (line, col) = LtToken (line, col)

let lteqSymbol (line, col) = LteqToken (line, col)

let gtSymbol (line, col) = GtToken (line, col)

let gteqSymbol (line, col) = GteqToken (line, col)

let andSymbol (line, col) = AndToken (line, col)

let orSymbol (line, col) = OrToken (line, col)

let assignSymbol (line, col) = AssignToken (line, col)
(*End symbol functions*)
