open Core

let () =
  let args = Array.to_list (Sys.get_argv ()) in
  match args with
  | _ :: path :: _ ->
    ( match Main.analyze_file ~path with
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
