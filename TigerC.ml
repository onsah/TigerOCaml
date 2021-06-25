(*Makes escaped characters visible in str*)
let escaped_str str =
  let chars =
    List.init (String.length str) (fun i -> Char.escaped (String.get str i))
  in
  String.concat "" chars

let loop lexbuf = Parser.program Lexer.token lexbuf
(* let result = Lexer.token lexbuf in
   Printf.printf "%s\n" (String.escaped result) ;
   match result with
   | "EOF" -> ()
   | _ -> loop lexbuf *)

let main filename =
  let file = open_in filename in
  let lexbuf = Lexing.from_channel file in
  loop lexbuf

let () =
  let expr = main "tiger.tig" in
  let str = Syntax.show_expr expr in
  Printf.printf "Ast: %s\n" str
  (* match expr with
    | StringExpr str -> Printf.printf "String: %s\n" str *)
