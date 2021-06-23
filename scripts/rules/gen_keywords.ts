const keywords = [
    "while", "for",
    "to",    "break",
    "let",   "in",
    "end",   "function",
    "var",   "type",
    "array", "if",
    "then",  "else",
    "do",    "of",
    "nil"
];  

const rules = keywords.map(kw => {
    const len = kw.length;
    const token = `${kw}Keyword`
    return (
`| "${kw}"
    {
        num_cols := !num_cols + ${len};
        Tokens.${token} (!num_lines, !num_cols)
    }`
);
});

export const genKeywords = (): string =>
    rules.reduce((prev, curr) => `${prev}\n${curr}`);

// rules.forEach(r => console.log(r));