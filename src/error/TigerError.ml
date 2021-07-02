module TigerError = struct
  
  exception LexError of string

  let lexError prefix chr line pos =
    match chr with
    | Some chr ->
        raise
          (LexError
             ( "[" ^ prefix ^ "] Failed to parse " ^ Char.escaped chr
             ^ " at line: " ^ string_of_int line ^ ", column: "
             ^ string_of_int pos ) )
    | None ->
        raise
          (LexError
             ( "[" ^ prefix ^ "] at line: " ^ string_of_int line ^ ", column: "
             ^ string_of_int pos ) )
end
