module type FRAME = sig
  type frame [@@deriving show]

  type access [@@deriving show]

  val newFrame : name:Temp.label -> formals:bool list -> frame

  val name : frame -> Temp.label

  val formals : frame -> access list

  val allocLocal : frame -> bool -> access
end

module MipsFrame : FRAME = struct
  type frame =
    { name : Temp.label
    ; formals : access list
    ; locals : access list ref
    }
  [@@deriving show]
  (* TODO *)

  and access =
    | InStack of int
    | InReg of Temp.temp
  [@@deriving show]
  (* TODO *)

  let mipsStackEntrySize = 4

  let mipsMaxRegister = 5

  let newFrame ~name ~formals =
    match List.length formals < mipsMaxRegister with
    | true ->
        { name
        ; formals =
            List.map
              (fun _ -> (* TODO: handle escape *) InReg (Temp.newtemp ()))
              formals
        ; locals = ref []
        }
    | false ->
        raise (Failure "Mips can't handle this amount parameters")


  let name frame = frame.name

  let formals frame = frame.locals.contents

  let allocLocal frame _ (* TODO: handle escapes *) =
    let localsLen = List.length frame.locals.contents in
    let newLocal = InStack (-mipsStackEntrySize * localsLen) in
    frame.locals := newLocal :: !(frame.locals) ;
    newLocal
end
