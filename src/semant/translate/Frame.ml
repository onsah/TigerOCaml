module type Frame = sig
  type frame [@@deriving show]

  type access [@@deriving show]

  val new_frame : name:Temp.label -> formals:bool list -> frame

  val name : frame -> Temp.label

  val formals : frame -> access list

  val alloc_local : frame -> bool -> access
end

module MipsFrame : Frame = struct
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

  let mips_stack_entry_size = 4

  let mips_max_register = 5

  let new_frame ~name ~formals =
    match List.length formals < mips_max_register with
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

  let alloc_local frame _ (* TODO: handle escapes *) =
    let locals_len = List.length frame.locals.contents in
    let stack_offset = (-mips_stack_entry_size * (locals_len + 1)) in
    let new_local = InStack stack_offset in
    frame.locals := new_local :: !(frame.locals) ;
    new_local
end
