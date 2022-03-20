open Ir

module type Frame = sig
  type frame [@@deriving show]

  type access [@@deriving show]

  type frag =
    | Proc of
        { body : IRTree.stmt
        ; frame : frame
        }
    | String of Temp.label * string
  [@@deriving show]

  val new_frame : name:Temp.label -> formals:bool list -> frame

  val name : frame -> Temp.label

  (* First access is static link *)
  val formals : frame -> access list

  val alloc_local : frame -> bool -> access

  val word_size : int

  (*Frame pointer*)
  val fp : Temp.temp

  (* Return value register *)
  val rv : Temp.temp

  val expr : access -> fp:IRTree.expr -> IRTree.expr

  type built_in_call =
    | StrEq (* stringEqual: string * string -> bool *)
    | StrLt (* stringLessThan: string * string -> bool *)
    | StrLte (* stringLessThanOrEqual: string * string -> bool *)
    | Malloc
    (* malloc: size: int -> pointer *)
    | InitArray
  (* initialize array: size: int * init_value: expr -> pointer *)

  val external_call : func:built_in_call -> args:IRTree.expr list -> IRTree.expr

  (* Generates assembly fragment for a string *)
  val string : label:Symbol.symbol -> literal:string -> unit
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

  and frag =
    | Proc of
        { body : IRTree.stmt
        ; frame : frame
        }
    | String of Temp.label * string
  [@@deriving show]

  let word_size = 4

  let fp = Temp.newtemp ()

  let rv = Temp.newtemp ()

  let expr access ~fp =
    match access with
    | InReg t ->
        IRTree.Temp t
    | InStack offset ->
        IRTree.Mem
          (IRTree.Binop
             { left = IRTree.Const offset; right = fp; op = IRTree.Plus } )


  let mips_max_register = 4

  let new_frame ~name ~formals =
    let _, formal_accesses =
      List.fold_left_map
        (fun (escape_so_far, stack_depth) escape ->
          match escape_so_far < mips_max_register && not escape with
          | true ->
              ((escape_so_far + 1, stack_depth), InReg (Temp.newtemp ()))
          | false ->
              let stack_offset = -word_size * stack_depth in
              ((escape_so_far, stack_depth + 1), InStack stack_offset) )
        (0, 0)
        formals
    in
    { name; formals = formal_accesses; locals = ref [] }


  let name frame = frame.name

  let formals frame = frame.formals

  let alloc_local frame escape =
    Printf.printf "Allocating: %B\n" escape ;
    let locals_len =
      List.length
        (List.filter
           (fun access ->
             match access with InStack _ -> true | InReg _ -> false )
           frame.locals.contents )
    in
    let stack_offset = -word_size * (locals_len + 1) in
    let new_local =
      match escape with
      | true ->
          InStack stack_offset
      | false ->
          InReg (Temp.newtemp ())
    in
    frame.locals := new_local :: !(frame.locals) ;
    new_local


  type built_in_call =
    | StrEq
    | StrLt
    | StrLte
    | Malloc
    | InitArray

  let external_call ~func ~args =
    let built_in_symbol func =
      let name =
        match func with
        | StrEq ->
            "stringEqual"
        | StrLt ->
            "stringLessThan"
        | StrLte ->
            "stringLessThanOrEqual"
        | Malloc ->
            "malloc"
        | InitArray ->
            "initArray"
      in
      Symbol.symbol name
    in
    let symbol = built_in_symbol func in
    IRTree.Call { func = IRTree.Name symbol; args }


  let string ~label:_ ~literal:_ = failwith "TODO: implement"
end
