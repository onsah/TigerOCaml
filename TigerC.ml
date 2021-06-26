open MenhirLib
open UnitActionsParser

(*Makes escaped characters visible in str*)
let escaped_str str =
  let chars =
    List.init (String.length str) (fun i -> Char.escaped (String.get str i))
  in
  String.concat "" chars

(*Fast pass, if no error results with better performance*)
let quickpass filename : (Syntax.expr, string) result =
  let text, lexbuf = LexerUtil.read filename in
  match Parser.program Lexer.token lexbuf with
  | expr ->
      Ok expr
  | exception Lexer.Error err ->
      Printf.eprintf "Error: %s" err ;
      exit 1
  | exception Parser.Error ->
      Error text

let show_error text positions =
  ErrorReports.extract text positions
  |> ErrorReports.sanitize |> ErrorReports.compress |> ErrorReports.shorten 20

let fail_handler text buffer (_ : _ MenhirInterpreter.checkpoint) =
  let location = LexerUtil.range (ErrorReports.last buffer) in
  let errorTokens =
    Printf.sprintf "Syntax error: %s"
      (ErrorReports.show (show_error text) buffer)
  in
  Printf.eprintf "%s%s!\n" location errorTokens ;
  exit 1

(*We only run this to find the error location in the parsing phase.*)
let incrementalpass filename text =
  let lexbuf = LexerUtil.init filename (Lexing.from_string text) in
  let supplier =
    MenhirInterpreter.lexer_lexbuf_to_supplier Lexer.token lexbuf
  in
  let buffer, supplier = ErrorReports.wrap_supplier supplier in
  let checkpoint = UnitActionsParser.Incremental.program lexbuf.lex_curr_p in
  MenhirInterpreter.loop_handle
    (fun _ -> assert false)
    (fail_handler text buffer) supplier checkpoint

let () =
  let args = Array.to_list Sys.argv in
  match args with
  | _ :: path :: _ -> (
    match quickpass path with
    | Ok expr ->
        Printf.printf "%s\n" (Syntax.show_expr expr)
    | Error text ->
        incrementalpass path text )
  | _ ->
      Printf.printf "[TigerC] Usage: tigerC <path>\n"
