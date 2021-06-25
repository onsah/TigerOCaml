import { genKeywords } from './gen_keywords.ts';
import { genEscapes } from './gen_escapes.ts';
import { genSymbols } from './gen_symbols.ts';

const lexerPath = "../../lexer.mll";

const text = Deno.readTextFileSync(lexerPath);

const [beforeKeywords, startKeywords] = text.split("(*Start keywords*)");
const [_2, startSymbols] = startKeywords.split("(*Start symbols*)");
const [_3, afterSymbols] = startSymbols.split("(*End symbols*)");
const [beforeEscapes, startEscapes] = afterSymbols.split("(*Start escapes*)");
const [_5, afterEscapes] = startEscapes.split("(*End escapes*)");

const keywords = genKeywords();
const escapes = genEscapes();
const symbols = genSymbols();

const result = `\
${beforeKeywords}\
(*Start keywords*)
${keywords}
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