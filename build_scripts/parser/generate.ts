import { symbols, keywords } from '../data.ts';
import { toSingleString } from '../utils.ts';

const parserPath = '../../parser.mly';
const text = Deno.readTextFileSync(parserPath);

const [ beforeSymbolTokens, startSymbolTokens ] = text.split('(*Start symbol tokens*)');
const [ _1, startKeywordTokens ] = startSymbolTokens.split('(*Start keyword tokens*)');
const [ _2, afterTokens ] = startKeywordTokens.split('(*End tokens*)');

const symbolTokens = toSingleString(symbols.map(([_, name]) => `%token ${name.toUpperCase()}`));
const keywordTokens = toSingleString(keywords.map(kw => `%token ${kw.toUpperCase()}`));

const result = 
`${beforeSymbolTokens}\
(*Start symbol tokens*)
${symbolTokens}
(*Start keyword tokens*)
${keywordTokens}
(*End tokens*)
${afterTokens}
`

Deno.writeTextFileSync(parserPath, result);