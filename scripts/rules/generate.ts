import { genKeywords } from './gen_keywords.ts';
import { genEscapes } from './gen_escapes.ts';
import { genSymbols } from './gen_symbols.ts';

const lexerPath = "../../lexer.mll";

const text = Deno.readTextFileSync(lexerPath);

const [beforeKeywords, _1] = text.split("(*Start keywords*)");
const [_2, afterKeywords] = _1.split("(*End keywords*)");
const [beforeSymbols, startSymbols] = afterKeywords.split("(*Start symbols*)");
const [_5, afterSymbols] = startSymbols.split("(*End symbols*)");
const [beforeEscapes, _3] = afterSymbols.split("(*Start escapes*)");
const [_4, afterEscapes] = _3.split("(*End escapes*)");

const keywords = genKeywords();
const escapes = genEscapes();
const symbols = genSymbols();

// console.log(symbols);

// console.log(`before: ${beforeKeywords}`)

const result = `\
${beforeKeywords}\
(*Start keywords*)
${keywords}
(*End keywords*)\
${beforeSymbols}\
(*Start symbols*)
${symbols}
(*End symbols*)\
${beforeEscapes}\
(*Start escapes*)
${escapes}
(*End escapes*)\
${afterEscapes}\
`;

Deno.writeTextFileSync(lexerPath, result);