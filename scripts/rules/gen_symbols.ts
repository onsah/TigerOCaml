const symbols = [
    [',', 'comma'], [':', 'colon'], 
    [';', 'semicolon'], ['(', 'lparen'], [')', 'rparen'], 
    ['[', 'lbrack'], [']', 'rbrack'], ['{', 'lcurly'], ['}', 'rcurly'], 
    ['.', 'dot'], ['+', 'plus'], ['-', 'minus'], 
    ['*', 'times'], ['/', 'div'], 
    ['=', 'eq'], ['<>', 'ltgt'], ['<', 'lt'], ['<=', 'lteq'], ['>', 'gt'], ['>=', 'gteq'], 
    ['&', 'and'], ['|', 'or'], [':=', 'assign'],
];

const rules = symbols.map(([symbol, name]) => {
    const len = symbol.length;

    return (
`| "${symbol}"
    {
        num_cols := !num_cols + ${len};
        Tokens.${name}Symbol (!num_lines, !num_cols)
    }`
)
});

export const genSymbols = (): string =>
    rules.reduce((prev, curr) => `${prev}\n${curr}`);

// console.log(genSymbols());