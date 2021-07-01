export const binaryOperators = [
    ['+', 'plus'], ['-', 'minus'], 
    ['*', 'times'], ['/', 'div'], 
    ['=', 'eq'], ['<>', 'ltgt'], ['<', 'lt'], ['<=', 'lteq'], ['>', 'gt'], ['>=', 'gteq'], 
    ['&', 'and'], ['|', 'or'], [':=', 'assign'],
];

export const symbols = [
    [',', 'comma'], [':', 'colon'], 
    [';', 'semicolon'], ['(', 'lparen'], [')', 'rparen'], 
    ['[', 'lbrack'], [']', 'rbrack'], ['{', 'lcurly'], ['}', 'rcurly'], 
    ['.', 'dot'], 
    ...binaryOperators
];

export const keywords = [
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