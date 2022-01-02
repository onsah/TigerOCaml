{
  open ParserYacc
  open TigerError
  open Syntax

  exception Error of string

  let num_lines = ref 1
  let num_cols = ref 1
  let start_lines = ref 1
  let start_cols = ref 1
  let str_content = ref ""
  let comment_level = ref 0

  let append_char str c = str ^ String.make 1 c

  let newLine lexbuf = 
    MenhirLib.LexerUtil.newline lexbuf;
    num_lines := !num_lines + 1;
    num_cols := 1

  let get_pos () = { line = !num_lines; col = !num_cols }

  let advance_and_return_start_pos length = 
    let pos = get_pos() 
    in 
        num_cols := !num_cols + length;
        pos
}

let digit = ['0'-'9']
let letter = ['a'-'z''A'-'Z']
let identifier = letter(digit | letter | '_')*
let newline = ['\n']
let formFeed = '\012'
let formatChars = [' ''\t''\012''\013']

rule token = parse
'"'
    {
      str_content := "";
      start_lines := !num_lines;
      start_cols := !num_cols;
      num_cols := !num_cols + 1;
      stringRule lexbuf
    }
| newline
    {
      newLine lexbuf;
      token lexbuf
    }
| formatChars
    {
      num_cols := !num_cols + 1;
      token lexbuf
    }
| digit+ as integer
    {
        start_cols := !num_cols;
        num_cols := !num_cols + (String.length integer);
        INT (TigerTokens.IntToken (int_of_string integer, !num_lines, !start_cols))  
    }
| "/*"
    {
      num_cols := !num_cols + 2;
      comment_level := !comment_level + 1;
      commentRule lexbuf
    }
| eof
    { EOF }
(*Start keywords*)
| "while"
    { WHILE (advance_and_return_start_pos 5) }
| "for"
    { FOR (advance_and_return_start_pos 3) }
| "to"
    {
        num_cols := !num_cols + 2;
        TO
    }
| "break"
    { BREAK (advance_and_return_start_pos 5) }
| "let"
    { LET (advance_and_return_start_pos 3) }
| "in"
    { IN (advance_and_return_start_pos 2) }
| "end"
    {
        num_cols := !num_cols + 3;
        END
    }
| "function"
    {
        num_cols := !num_cols + 8;
        FUNCTION
    }
| "var"
    { VAR (advance_and_return_start_pos 3) }
| "type"
    { TYPE (advance_and_return_start_pos 4) }
| "array"
    { ARRAY (advance_and_return_start_pos 5) }
| "if"
    { IF (advance_and_return_start_pos 2) }
| "then"
    {
        num_cols := !num_cols + 4;
        THEN
    }
| "else"
    {
        num_cols := !num_cols + 4;
        ELSE
    }
| "do"
    {
        num_cols := !num_cols + 2;
        DO
    }
| "of"
    {
        num_cols := !num_cols + 2;
        OF
    }
| "nil"
    {
        let pos = get_pos () in
            num_cols := !num_cols + 3;
            NIL (pos)
    }
(*Start symbols*)
| ","
    {
        num_cols := !num_cols + 1;
        COMMA
    }
| ":"
    {
        num_cols := !num_cols + 1;
        COLON
    }
| ";"
    {
        num_cols := !num_cols + 1;
        SEMICOLON
    }
| "("
    {
        LPAREN (advance_and_return_start_pos 1)
    }
| ")"
    {
        num_cols := !num_cols + 1;
        RPAREN
    }
| "["
    { LBRACK (advance_and_return_start_pos 1) }
| "]"
    {
        num_cols := !num_cols + 1;
        RBRACK
    }
| "{"
    { LCURLY (advance_and_return_start_pos 1) }
| "}"
    {
        num_cols := !num_cols + 1;
        RCURLY
    }
| "." 
    { DOT (advance_and_return_start_pos 1) }
| "+" 
    { PLUS (advance_and_return_start_pos 1) }
| "-"
    { MINUS (advance_and_return_start_pos 1) }
| "*" 
    { TIMES (advance_and_return_start_pos 1) }
| "/" 
    { DIV (advance_and_return_start_pos 1) }
| "=" 
    { EQ (advance_and_return_start_pos 1) }
| "<>" 
    { LTGT (advance_and_return_start_pos 2) }
| "<"
    { LT (advance_and_return_start_pos 1) }
| "<="
    { LTEQ (advance_and_return_start_pos 1) }
| ">"
    { GT (advance_and_return_start_pos 1) }
| ">="
    { GTEQ (advance_and_return_start_pos 2) }
| "&"
    { AND (advance_and_return_start_pos 1) }
| "|"
    { OR (advance_and_return_start_pos 1) }
| ":="
    { ASSIGN (advance_and_return_start_pos 2) }
| identifier as identifier
    {
        let { line; col } = get_pos() in
            num_cols := !num_cols + (String.length identifier);
            IDENT (TigerTokens.IdentToken (identifier, line, col))
    }
(*End symbols*)
| _ as chr
    {
      TigerError.lexError "unexpected character" (Some chr) !num_lines !num_cols
    }
and stringRule = parse 
| '"'
    {
      num_cols := !num_cols + 1;
      STRING (TigerTokens.StringToken (!str_content, !start_lines, !start_cols))
    }
| newline as c
    {
      newLine lexbuf;
      str_content := append_char !str_content c;
      stringRule lexbuf
    }
| '\\'
    {
      num_cols := !num_cols + 1;  
      escapeRule lexbuf
    }
| eof
    {
      TigerError.lexError "Unclosed string" None !num_lines !num_cols
    }
| _ as c
    {
      num_cols := !num_cols + 1;  
      str_content := append_char !str_content c;
      stringRule lexbuf
    }
and escapeRule = parse
| 'n'
    {
      num_cols := !num_cols + 1;  
      str_content := append_char !str_content '\n';
      stringRule lexbuf
    }
| 't'
    {
      num_cols := !num_cols + 1;  
      str_content := append_char !str_content '\t';
      stringRule lexbuf
    }
| '^'
    {
      controlRule lexbuf
    }
| digit digit digit as ascii
    {
      num_cols := !num_cols + 3;
      (*TODO: handle char exceeds ascii value*)
      str_content := append_char !str_content (Char.chr (int_of_string ascii));
      stringRule lexbuf
    }
| '\"'
    {
      num_cols := !num_cols + 1;  
      str_content := append_char !str_content '\"';
      stringRule lexbuf
    }
| '\\'
    {
      num_cols := !num_cols + 1;  
      str_content := append_char !str_content '\\';
      stringRule lexbuf
    }
| 'f'
    {
      num_cols := !num_cols + 1;  
      formatRule lexbuf
    }
| eof 
    {
      TigerError.lexError "Unclosed string" None !num_lines !num_cols
    }
| _ as chr
    {
      TigerError.lexError "invalid escape rule" (Some chr) !num_lines !num_cols
    }
and controlRule = parse
(*Start escapes*)
| '@'
    {
      num_cols := !num_cols + 1;
      str_content := append_char !str_content '\000';
      stringRule lexbuf
    }

| 'A'
    {
      num_cols := !num_cols + 1;
      str_content := append_char !str_content '\001';
      stringRule lexbuf
    }

| 'B'
    {
      num_cols := !num_cols + 1;
      str_content := append_char !str_content '\002';
      stringRule lexbuf
    }

| 'C'
    {
      num_cols := !num_cols + 1;
      str_content := append_char !str_content '\003';
      stringRule lexbuf
    }

| 'D'
    {
      num_cols := !num_cols + 1;
      str_content := append_char !str_content '\004';
      stringRule lexbuf
    }

| 'E'
    {
      num_cols := !num_cols + 1;
      str_content := append_char !str_content '\005';
      stringRule lexbuf
    }

| 'F'
    {
      num_cols := !num_cols + 1;
      str_content := append_char !str_content '\006';
      stringRule lexbuf
    }

| 'G'
    {
      num_cols := !num_cols + 1;
      str_content := append_char !str_content '\007';
      stringRule lexbuf
    }

| 'H'
    {
      num_cols := !num_cols + 1;
      str_content := append_char !str_content '\008';
      stringRule lexbuf
    }

| 'I'
    {
      num_cols := !num_cols + 1;
      str_content := append_char !str_content '\009';
      stringRule lexbuf
    }

| 'J'
    {
      num_cols := !num_cols + 1;
      str_content := append_char !str_content '\010';
      stringRule lexbuf
    }

| 'K'
    {
      num_cols := !num_cols + 1;
      str_content := append_char !str_content '\011';
      stringRule lexbuf
    }

| 'L'
    {
      num_cols := !num_cols + 1;
      str_content := append_char !str_content '\012';
      stringRule lexbuf
    }

| 'M'
    {
      num_cols := !num_cols + 1;
      str_content := append_char !str_content '\013';
      stringRule lexbuf
    }

| 'N'
    {
      num_cols := !num_cols + 1;
      str_content := append_char !str_content '\014';
      stringRule lexbuf
    }

| 'O'
    {
      num_cols := !num_cols + 1;
      str_content := append_char !str_content '\015';
      stringRule lexbuf
    }

| 'P'
    {
      num_cols := !num_cols + 1;
      str_content := append_char !str_content '\016';
      stringRule lexbuf
    }

| 'Q'
    {
      num_cols := !num_cols + 1;
      str_content := append_char !str_content '\017';
      stringRule lexbuf
    }

| 'R'
    {
      num_cols := !num_cols + 1;
      str_content := append_char !str_content '\018';
      stringRule lexbuf
    }

| 'S'
    {
      num_cols := !num_cols + 1;
      str_content := append_char !str_content '\019';
      stringRule lexbuf
    }

| 'T'
    {
      num_cols := !num_cols + 1;
      str_content := append_char !str_content '\020';
      stringRule lexbuf
    }

| 'U'
    {
      num_cols := !num_cols + 1;
      str_content := append_char !str_content '\021';
      stringRule lexbuf
    }

| 'V'
    {
      num_cols := !num_cols + 1;
      str_content := append_char !str_content '\022';
      stringRule lexbuf
    }

| 'W'
    {
      num_cols := !num_cols + 1;
      str_content := append_char !str_content '\023';
      stringRule lexbuf
    }

| 'X'
    {
      num_cols := !num_cols + 1;
      str_content := append_char !str_content '\024';
      stringRule lexbuf
    }

| 'Y'
    {
      num_cols := !num_cols + 1;
      str_content := append_char !str_content '\025';
      stringRule lexbuf
    }

| 'Z'
    {
      num_cols := !num_cols + 1;
      str_content := append_char !str_content '\026';
      stringRule lexbuf
    }

| '['
    {
      num_cols := !num_cols + 1;
      str_content := append_char !str_content '\027';
      stringRule lexbuf
    }

| ']'
    {
      num_cols := !num_cols + 1;
      str_content := append_char !str_content '\028';
      stringRule lexbuf
    }

| '^'
    {
      num_cols := !num_cols + 1;
      str_content := append_char !str_content '\029';
      stringRule lexbuf
    }

| '_'
    {
      num_cols := !num_cols + 1;
      str_content := append_char !str_content '\030';
      stringRule lexbuf
    }

| ' '
    {
      num_cols := !num_cols + 1;
      str_content := append_char !str_content '\031';
      stringRule lexbuf
    }

(*End escapes*)
| eof 
    {
      TigerError.lexError "Unclosed string" None !num_lines !num_cols
    }
| _ as chr
    {
      TigerError.lexError "invalid control escape" (Some chr) !num_lines !num_cols
    }
and formatRule = parse
| newline
    {
      newLine lexbuf;
      formatRule lexbuf
    }
| formatChars
    {
      num_cols := !num_cols + 1;
      formatRule lexbuf
    }
| "f\\"
    {
      num_cols := !num_cols + 2;
      stringRule lexbuf
    }
| eof 
    {
      TigerError.lexError "Unclosed string" None !num_lines !num_cols
    }
| _ as chr
    {
      TigerError.lexError "invalid format character" (Some chr) !num_lines !num_cols
    }
and commentRule = parse
| "*/"
    {
      num_cols := !num_cols + 2;
      if !comment_level > 0 then
        (comment_level := !comment_level - 1;
        if !comment_level == 0 then
          token lexbuf
        else 
          commentRule lexbuf)
      else
        TigerError.lexError "Unmatched comment close" None !num_lines !num_cols
    }
| "/*"
    {
      num_cols := !num_cols + 2;
      comment_level := !comment_level + 1;
      commentRule lexbuf
    }
| _ 
    {
      num_cols := !num_cols + 1;
      commentRule lexbuf
    }
| eof
    {
      TigerError.lexError "Unclosed comment" None !num_lines !num_cols
    }