open Core

let parse_file = Parser.parse_file

let analyze_file ~path =
  Result.map (parse_file ~path) ~f:(fun expr ->
      ignore (FindEscape.find_escape expr) ;
      ignore (Printf.printf "%s\n" (Syntax.show_expr expr)) ;
      Semant.type_check expr )
