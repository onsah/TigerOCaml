import { keywords } from '../data.ts';

const rules = keywords.map(kw => {
    const len = kw.length;
    const token = `${kw}Keyword`
    return (
`| "${kw}"
    {
        num_cols := !num_cols + ${len};
        ${kw.toUpperCase()}
    }`
);
});

// Tokens.${token} (!num_lines, !num_cols)

export const genKeywords = (): string =>
    rules.reduce((prev, curr) => `${prev}\n${curr}`);