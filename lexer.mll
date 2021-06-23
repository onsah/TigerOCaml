{
  open Tokens
  open TigerError

  exception Error of string

  let num_lines = ref 1
  let num_cols = ref 1
  let str_content = ref ""
  let comment_level = ref 0

  let append_char str c = str ^ String.make 1 c
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
      stringRule lexbuf
    }
| newline
    {
      num_lines := !num_lines + 1;
      num_cols := 0;
      token lexbuf
    }
| formatChars
    {
      num_cols := !num_cols + 1;
      token lexbuf
    }
| "/*"
    {
      num_cols := !num_cols + 2;
      comment_level := !comment_level + 1;
      commentRule lexbuf
    }
| digit+ as integer
    {
      num_cols := !num_cols + (String.length integer);
      Tokens.integer (int_of_string integer, !num_lines, !num_cols)  
    }
(*Start keywords*)
| "while"
    {
        num_cols := !num_cols + 5;
        Tokens.whileKeyword (!num_lines, !num_cols)
    }
| "for"
    {
        num_cols := !num_cols + 3;
        Tokens.forKeyword (!num_lines, !num_cols)
    }
| "to"
    {
        num_cols := !num_cols + 2;
        Tokens.toKeyword (!num_lines, !num_cols)
    }
| "break"
    {
        num_cols := !num_cols + 5;
        Tokens.breakKeyword (!num_lines, !num_cols)
    }
| "let"
    {
        num_cols := !num_cols + 3;
        Tokens.letKeyword (!num_lines, !num_cols)
    }
| "in"
    {
        num_cols := !num_cols + 2;
        Tokens.inKeyword (!num_lines, !num_cols)
    }
| "end"
    {
        num_cols := !num_cols + 3;
        Tokens.endKeyword (!num_lines, !num_cols)
    }
| "function"
    {
        num_cols := !num_cols + 8;
        Tokens.functionKeyword (!num_lines, !num_cols)
    }
| "var"
    {
        num_cols := !num_cols + 3;
        Tokens.varKeyword (!num_lines, !num_cols)
    }
| "type"
    {
        num_cols := !num_cols + 4;
        Tokens.typeKeyword (!num_lines, !num_cols)
    }
| "array"
    {
        num_cols := !num_cols + 5;
        Tokens.arrayKeyword (!num_lines, !num_cols)
    }
| "if"
    {
        num_cols := !num_cols + 2;
        Tokens.ifKeyword (!num_lines, !num_cols)
    }
| "then"
    {
        num_cols := !num_cols + 4;
        Tokens.thenKeyword (!num_lines, !num_cols)
    }
| "else"
    {
        num_cols := !num_cols + 4;
        Tokens.elseKeyword (!num_lines, !num_cols)
    }
| "do"
    {
        num_cols := !num_cols + 2;
        Tokens.doKeyword (!num_lines, !num_cols)
    }
| "of"
    {
        num_cols := !num_cols + 2;
        Tokens.ofKeyword (!num_lines, !num_cols)
    }
| "nil"
    {
        num_cols := !num_cols + 3;
        Tokens.nilKeyword (!num_lines, !num_cols)
    }
(*End keywords*)
| identifier as identifier
    {
      num_cols := !num_cols + (String.length identifier);
      Tokens.identifier (identifier, !num_lines, !num_cols)
    }
(*Start symbols*)
| ","
    {
        num_cols := !num_cols + 1;
        Tokens.commaSymbol (!num_lines, !num_cols)
    }
| ":"
    {
        num_cols := !num_cols + 1;
        Tokens.colonSymbol (!num_lines, !num_cols)
    }
| ";"
    {
        num_cols := !num_cols + 1;
        Tokens.semicolonSymbol (!num_lines, !num_cols)
    }
| "("
    {
        num_cols := !num_cols + 1;
        Tokens.lparenSymbol (!num_lines, !num_cols)
    }
| ")"
    {
        num_cols := !num_cols + 1;
        Tokens.rparenSymbol (!num_lines, !num_cols)
    }
| "["
    {
        num_cols := !num_cols + 1;
        Tokens.lbrackSymbol (!num_lines, !num_cols)
    }
| "]"
    {
        num_cols := !num_cols + 1;
        Tokens.rbrackSymbol (!num_lines, !num_cols)
    }
| "{"
    {
        num_cols := !num_cols + 1;
        Tokens.lcurlySymbol (!num_lines, !num_cols)
    }
| "}"
    {
        num_cols := !num_cols + 1;
        Tokens.rcurlySymbol (!num_lines, !num_cols)
    }
| "."
    {
        num_cols := !num_cols + 1;
        Tokens.dotSymbol (!num_lines, !num_cols)
    }
| "+"
    {
        num_cols := !num_cols + 1;
        Tokens.plusSymbol (!num_lines, !num_cols)
    }
| "-"
    {
        num_cols := !num_cols + 1;
        Tokens.minusSymbol (!num_lines, !num_cols)
    }
| "*"
    {
        num_cols := !num_cols + 1;
        Tokens.timesSymbol (!num_lines, !num_cols)
    }
| "/"
    {
        num_cols := !num_cols + 1;
        Tokens.divSymbol (!num_lines, !num_cols)
    }
| "="
    {
        num_cols := !num_cols + 1;
        Tokens.eqSymbol (!num_lines, !num_cols)
    }
| "<>"
    {
        num_cols := !num_cols + 2;
        Tokens.ltgtSymbol (!num_lines, !num_cols)
    }
| "<"
    {
        num_cols := !num_cols + 1;
        Tokens.ltSymbol (!num_lines, !num_cols)
    }
| "<="
    {
        num_cols := !num_cols + 2;
        Tokens.lteqSymbol (!num_lines, !num_cols)
    }
| ">"
    {
        num_cols := !num_cols + 1;
        Tokens.gtSymbol (!num_lines, !num_cols)
    }
| ">="
    {
        num_cols := !num_cols + 2;
        Tokens.gteqSymbol (!num_lines, !num_cols)
    }
| "&"
    {
        num_cols := !num_cols + 1;
        Tokens.andSymbol (!num_lines, !num_cols)
    }
| "|"
    {
        num_cols := !num_cols + 1;
        Tokens.orSymbol (!num_lines, !num_cols)
    }
| ":="
    {
        num_cols := !num_cols + 2;
        Tokens.assignSymbol (!num_lines, !num_cols)
    }
(*End symbols*)
| eof
    { "EOF" }
| _ as chr
    {
      TigerError.lexError "unexpected character" (Some chr) !num_lines !num_cols
    }
and stringRule = parse 
| '"'
    {
      num_cols := !num_cols + 1;
      Tokens.string (!str_content, !num_lines, !num_cols)
    }
| '\n' as c
    {
      num_lines := !num_lines + 1;
      num_cols := 0;
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
    (* handles ASCII control codes *)
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
      num_lines := !num_lines + 1;
      num_cols := 0;
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