const keywords = [
  "while",
  "for",
  "to",
  "break",
  "let",
  "in",
  "end",
  "function",
  "var",
  "type",
  "array",
  "if",
  "then",
  "else",
  "do",
  "of",
  "nil",
];

const getKeywordTokens = () =>
  keywords.map((kw) => {
    const camelCased = `${kw[0].toUpperCase()}${kw.slice(1)}`;
    return `let ${kw}Keyword (line, col) = (String.concat ("")) [ "${camelCased}: "; string_of_int line ; ","; string_of_int col ] ;;`;
  });

const keywordFns = getKeywordTokens().reduce((prev, curr) => `${prev}\n${curr}`);

const symbols = [
  [',', 'comma'], [':', 'colon'], 
  [';', 'semicolon'], ['(', 'lparen'], [')', 'rparen'], 
  ['[', 'lbrack'], [']', 'rbrack'], ['{', 'lcurly'], ['}', 'rcurly'], 
  ['.', 'dot'], ['+', 'plus'], ['-', 'minus'], 
  ['*', 'times'], ['/', 'div'], 
  ['=', 'eq'], ['<>', 'ltgt'], ['<', 'lt'], ['<=', 'lteq'], ['>', 'gt'], ['>=', 'gteq'], 
  ['&', 'and'], ['|', 'or'], [':=', 'assign'],
];

const getSymbolTokens = () => 
  symbols.map(([_, name]) => {
    const capitalized = name.toUpperCase();
    return `let ${name}Symbol (line, col) = (String.concat ("")) [ "${capitalized}: "; string_of_int line ; ","; string_of_int col ] ;;`;
  });

const symbolFns = getSymbolTokens().reduce((prev, curr) => `${prev}\n${curr}`);

const tokensPath = "../../Tokens.ml";

const text = Deno.readTextFileSync(tokensPath);

const [beforeKeywords, _1] = text.split("(*Start keywords*)");
const [_2, afterKeywords] = _1.split("(*End keywords*)");
const [ beforeSymbols, startSymbols ] = afterKeywords.split("(*Start symbols*)");
const [_3, afterSymbols] = startSymbols.split("(*End symbols*)");

const result = `\
${beforeKeywords}\
(*Start keywords*)
${keywordFns}
(*End keywords*)\
${beforeSymbols}\
(*Start symbols*)
${symbolFns}
(*End symbols*)
${afterSymbols}`;

Deno.writeTextFileSync(tokensPath, result);