open Core

let () =
  let args = Array.to_list (Sys.get_argv ()) in
  match args with
  | _ :: path :: _ ->
      let expr =
        match Parser.parse_file ~path with
        | Ok expr ->
            expr
        | Error err ->
            eprintf
              "Error : %s\n\n%s\n"
              (Exn.to_string err)
              (Printexc.get_backtrace ()) ;
            exit 1
      in
      ignore (FindEscape.find_escape expr) ;
      ignore (Printf.printf "%s\n" (Syntax.show_expr expr)) ;
      let typed_expr = Semant.type_check expr in
      Printf.printf "%s\n" (Translated.show_typedExpr typed_expr)
  | _ ->
      Printf.printf "[TigerC] Usage: tigerC <path>\n"
