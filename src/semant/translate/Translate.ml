module Frame = Frame.MipsFrame

type access = level * Frame.access [@@deriving show]

and level =
  { parent : level option
  ; frame : Frame.frame
  }
[@@deriving show]

let outermost =
  { parent = None
  ; frame = Frame.newFrame ~name:(Temp.newlabel ()) ~formals:[]
  }


let newLevel ~parent ~name ~formals =
  let formalsEscape = List.map (fun _ -> true) formals in
  { parent = Some parent
  ; frame =
      Frame.newFrame ~name ~formals:(true (* pass static link as an argument *) :: formalsEscape)
  }


let formals level = List.map (fun frameAccess -> (level, frameAccess)) (List.tl (Frame.formals level.frame))

let allocLocal level escape = (level, Frame.allocLocal level.frame escape)


type expr = unit [@@deriving show]