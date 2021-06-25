import { keywords, symbols } from '../data.ts';
import { camelCased, toSingleString } from '../utils.ts';

const getKeywordTypes = () =>
  keywords.map(kw => 
    `type ${kw}Token = ${camelCased(kw)}Token of int * int`);

const keywordTypes = toSingleString(getKeywordTypes());

const getKeywordFns = () =>
  keywords.map((kw) =>
    `let ${kw}Keyword (line, col) = ${camelCased(kw)}Token (line, col)`
  );

const keywordFns = toSingleString(getKeywordFns());

const getSymbolTypes = () => 
  symbols.map(([_, name]) =>
    `type ${name}Token = ${camelCased(name)}Token of int * int`
  );

const symbolTypes = toSingleString(getSymbolTypes());

const getSymbolFns = () => 
  symbols.map(([_, name]) =>
    `let ${name}Symbol (line, col) = ${camelCased(name)}Token (line, col)`
  );

const symbolFns = toSingleString(getSymbolFns());

const tokensPath = "../../TigerTokens.ml";

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
