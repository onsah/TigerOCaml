import { symbols } from '../data.ts';

const rules = symbols.map(([symbol, name]) => {
    const len = symbol.length;

    return (
`| "${symbol}"
    {
        num_cols := !num_cols + ${len};
        ${name.toUpperCase()}
    }`
)
});
// Tokens.${name}Symbol (!num_lines, !num_cols)

export const genSymbols = (): string =>
    rules.reduce((prev, curr) => `${prev}\n${curr}`);