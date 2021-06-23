(*Makes escaped characters visible in str*)
let escaped_str str = 
  let chars = List.init (String.length str) (fun i -> Char.escaped (String.get str i)) in
  String.concat "" chars

let rec loop lexbuf =
  let result = Lexer.token lexbuf in
  Printf.printf "%s\n" (String.escaped result) ;
  match result with
  | "EOF" -> ()
  | _ -> loop lexbuf

let main filename =
    let file = open_in filename in
    let lexbuf = Lexing.from_channel file in
    loop lexbuf


let () =
    main "Files/tiger/testcases/merge.tig"