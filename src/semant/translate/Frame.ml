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

  and access =
    | InStack of int
    | InReg of Temp.temp
  [@@deriving show]

  let mips_stack_entry_size = 4

  let mips_max_register = 4

  let new_frame ~name ~formals =
    let _, formal_accesses =
      List.fold_left_map
        (fun (escape_so_far, stack_depth) escape ->
          match escape_so_far < mips_max_register && not escape with
          | true ->
              ((escape_so_far + 1, stack_depth), InReg (Temp.newtemp ()))
          | false ->
              let stack_offset = -mips_stack_entry_size * stack_depth in
              ((escape_so_far, stack_depth + 1), InStack stack_offset) )
        (0, 0)
        formals
    in
    { name; formals = formal_accesses; locals = ref [] }


  let name frame = frame.name

  let formals frame = frame.locals.contents

  let alloc_local frame escape (* TODO: handle escapes *) =
    Printf.printf "Allocating: %B\n" escape ;
    let locals_len =
      List.length
        (List.filter
           (fun access ->
             match access with InStack _ -> true | InReg _ -> false )
           frame.locals.contents )
    in
    let stack_offset = -mips_stack_entry_size * (locals_len + 1) in
    let new_local =
      match escape with
      | true ->
          InStack stack_offset
      | false ->
          InReg (Temp.newtemp ())
    in
    frame.locals := new_local :: !(frame.locals) ;
    new_local
end
