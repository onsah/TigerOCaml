open Core

let compile path =
  Result.map (Parser.parse_file ~path) ~f:(fun expr ->
      ignore (FindEscape.find_escape expr) ;
      ignore (Printf.printf "%s\n" (Syntax.show_expr expr)) ;
      Semant.type_check expr )


let () =
  let args = Array.to_list (Sys.get_argv ()) in
  match args with
  | _ :: path :: _ ->
    ( match compile path with
    | Ok typed_expr ->
        Printf.printf "%s\n" (Translated.show_typedExpr typed_expr)
    | Error err ->
        eprintf
          "Error : %s\n\n%s\n"
          (Exn.to_string err)
          (Printexc.get_backtrace ()) ;
        exit 1 )
  | _ ->
      Printf.printf "[TigerC] Usage: tigerC <path>\n"
