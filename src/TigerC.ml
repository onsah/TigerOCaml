open Core

let () =
  let args = Array.to_list (Sys.get_argv ()) in
  match args with
  | _ :: path :: _ ->
      let expr = Parser.parse_file ~path in
      ignore (FindEscape.find_escape expr) ;
      let () = Printf.printf "%s\n" (Syntax.show_expr expr) in
      let typed_expr = Semant.type_check expr in
      Printf.printf "%s\n" (Translated.show_typedExpr typed_expr)
  | _ ->
      Printf.printf "[TigerC] Usage: tigerC <path>\n"
