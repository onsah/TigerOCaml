open Core
open MenhirLib
open UnitActionsParser
open TigerError

let show_error text positions =
  ErrorReports.extract text positions
  |> ErrorReports.sanitize
  |> ErrorReports.compress
  |> ErrorReports.shorten 20


let fail_handler text buffer (_ : _ MenhirInterpreter.checkpoint) =
  let location = LexerUtil.range (ErrorReports.last buffer) in
  let errorTokens =
    Printf.sprintf
      "Syntax error: %s"
      (ErrorReports.show (show_error text) buffer)
  in
  Printf.eprintf "%s%s!\n" location errorTokens ;
  exit 1


let rec parse_file ~path =
  let text = In_channel.read_all path in
  match quickpass text with
  | Ok expr ->
      Ok expr
  | Error (TigerError.ParseError _) ->
      Ok (incrementalpass path text)
  | Error err ->
      Error err


(*Fast pass, if no error results with better performance*)
and quickpass text : (Syntax.expr, exn) result =
  let lexbuf = Lexing.from_string text in
  match ParserYacc.program Lexer.token lexbuf with
  | expr ->
      Ok expr
  | exception exn ->
      Error exn


(*We only run this to find the error location in the parsing phase.*)
and incrementalpass filename text =
  let lexbuf = LexerUtil.init filename (Lexing.from_string text) in
  let supplier =
    MenhirInterpreter.lexer_lexbuf_to_supplier Lexer.token lexbuf
  in
  let buffer, supplier = ErrorReports.wrap_supplier supplier in
  let checkpoint = UnitActionsParser.Incremental.program lexbuf.lex_curr_p in
  MenhirInterpreter.loop_handle
    (fun _ -> TigerError.unreachable ())
    (fail_handler text buffer)
    supplier
    checkpoint
