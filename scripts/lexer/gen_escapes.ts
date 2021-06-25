const escapes = [
    '@',
    ...Array.from({length: 26}, (_, i) => String.fromCharCode('A'.charCodeAt(0) + i)),
    '[', ']', '^', '_', ' '
];

const rules = escapes.map((c, i) => {
    const paddedAscii = `${i}`.padStart(2, '0');
    return (
`| '${c}'
    {
      num_cols := !num_cols + 1;
      str_content := append_char !str_content '\\0${paddedAscii}';
      stringRule lexbuf
    }
`)
});

export const genEscapes = (): string =>
    rules.reduce((prev, curr) => `${prev}\n${curr}`);

// rules.forEach(x => console.log(x));