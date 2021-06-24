/**
 * Auxuliary functions
 */
 const toSingleString = (arr: string[]): string =>
  arr.reduce((prev, curr) => `${prev}\n${curr}`);

 const camelCased = (str: string): string =>
  `${str[0].toUpperCase()}${str.slice(1)}`;

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

const getKeywordTypes = () =>
  keywords.map(kw => 
    `type ${kw}Token = ${camelCased(kw)}Token of int * int`);

const keywordTypes = toSingleString(getKeywordTypes());

const getKeywordFns = () =>
  keywords.map((kw) =>
    `let ${kw}Keyword (line, col) = ${camelCased(kw)}Token (line, col)`
  );

const keywordFns = getKeywordFns().reduce((prev, curr) => `${prev}\n${curr}`);

const symbols = [
  [',', 'comma'], [':', 'colon'], 
  [';', 'semicolon'], ['(', 'lparen'], [')', 'rparen'], 
  ['[', 'lbrack'], [']', 'rbrack'], ['{', 'lcurly'], ['}', 'rcurly'], 
  ['.', 'dot'], ['+', 'plus'], ['-', 'minus'], 
  ['*', 'times'], ['/', 'div'], 
  ['=', 'eq'], ['<>', 'ltgt'], ['<', 'lt'], ['<=', 'lteq'], ['>', 'gt'], ['>=', 'gteq'], 
  ['&', 'and'], ['|', 'or'], [':=', 'assign'],
];

const getSymbolTypes = () => 
  symbols.map(([_, name]) =>
    `type ${name}Token = ${camelCased(name)}Token of int * int`
  );

const symbolTypes = toSingleString(getSymbolTypes());

const getSymbolFns = () => 
  symbols.map(([_, name]) =>
    `let ${name}Symbol (line, col) = ${camelCased(name)}Token (line, col)`
  );

const symbolFns = getSymbolFns().reduce((prev, curr) => `${prev}\n${curr}`);

const tokensPath = "../../Tokens.ml";

const text = Deno.readTextFileSync(tokensPath);

// Type declarations
const [ beforeKeywordTokens, startKeywordTokens ] = text.split("(*Start keyword types*)");
const [ __1, afterSymbolTokens ] = startKeywordTokens.split("(*Start symbol types*)");
const [ __2, afterTokens ] = afterSymbolTokens.split("(*End types*)");

// generator functions
const [beforeKeywords, _1] = afterTokens.split("(*Start keyword functions*)");
const [_2, afterKeywords] = _1.split("(*End keyword functions*)");
const [ beforeSymbols, startSymbols ] = afterKeywords.split("(*Start symbol functions*)");
const [_3, afterSymbols] = startSymbols.split("(*End symbol functions*)");

const result = `\
${beforeKeywordTokens}
(*Start keyword types*)
${keywordTypes}
(*Start symbol types*)
${symbolTypes}
(*End types*)
${beforeKeywords}\
(*Start keyword functions*)
${keywordFns}
(*End keyword functions*)\
${beforeSymbols}\
(*Start symbol functions*)
${symbolFns}
(*End symbol functions*)
${afterSymbols}`;

Deno.writeTextFileSync(tokensPath, result);
// console.log(result)
