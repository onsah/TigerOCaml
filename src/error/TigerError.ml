open Syntax

module TigerError = struct
  exception LexError of string

  exception ParseError of string

  exception SemantError of string

  let unreachable () = raise (Failure "Unreachable")

  let lexError prefix chr line pos =
    match chr with
    | Some chr ->
        LexError
          ( "["
          ^ prefix
          ^ "] Failed to parse "
          ^ Char.escaped chr
          ^ " at line: "
          ^ string_of_int line
          ^ ", column: "
          ^ string_of_int pos )
    | None ->
        LexError
          ( "["
          ^ prefix
          ^ "] at line: "
          ^ string_of_int line
          ^ ", column: "
          ^ string_of_int pos )


  let semant_error (msg, pos) =
    let { line; col } = pos in
    raise
      (SemantError
         (msg ^ ": at line " ^ string_of_int line ^ ", col " ^ string_of_int col)
      )
end
