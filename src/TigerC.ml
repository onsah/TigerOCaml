open Core
open MenhirLib
open UnitActionsParser
open TigerError

(*Fast pass, if no error results with better performance*)
let quickpass text : (Syntax.expr, unit) result =
  let lexbuf = Lexing.from_string text in
  match Parser.program Lexer.token lexbuf with
  | expr -> Ok expr
  | exception Lexer.Error err ->
    Printf.eprintf "Error: %s" err;
    exit 1
  | exception Parser.Error -> Error ()
;;

let show_error text positions =
  ErrorReports.extract text positions
  |> ErrorReports.sanitize
  |> ErrorReports.compress
  |> ErrorReports.shorten 20
;;

let fail_handler text buffer (_ : _ MenhirInterpreter.checkpoint) =
  let location = LexerUtil.range (ErrorReports.last buffer) in
  let errorTokens =
    Printf.sprintf "Syntax error: %s" (ErrorReports.show (show_error text) buffer)
  in
  Printf.eprintf "%s%s!\n" location errorTokens;
  exit 1
;;

(*We only run this to find the error location in the parsing phase.*)
let incrementalpass filename text =
  let lexbuf = LexerUtil.init filename (Lexing.from_string text) in
  let supplier = MenhirInterpreter.lexer_lexbuf_to_supplier Lexer.token lexbuf in
  let buffer, supplier = ErrorReports.wrap_supplier supplier in
  let checkpoint = UnitActionsParser.Incremental.program lexbuf.lex_curr_p in
  MenhirInterpreter.loop_handle
    (fun _ -> TigerError.unreachable ())
    (fail_handler text buffer)
    supplier
    checkpoint
;;

let parse path =
  let text = In_channel.read_all path in
  match quickpass text with
  | Ok expr -> expr
  | Error _ -> incrementalpass path text
;;

let () =
  let args = Array.to_list (Sys.get_argv ()) in
  match args with
  | _ :: path :: _ ->
    let expr = parse path in
    let _ = print_string (Syntax.show_expr expr) in
    let typed_expr = Semant.type_check expr in
    Printf.printf "%s\n" (Translated.show_typedExpr typed_expr)
  | _ -> Printf.printf "[TigerC] Usage: tigerC <path>\n"
;;
