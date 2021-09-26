module Frame: Frame.Frame = Frame.MipsFrame

type access = level * Frame.access [@@deriving show]

and level =
  { parent : level option
  ; frame : Frame.frame
  }
[@@deriving show]

let outermost =
  { parent = None
  ; frame = Frame.new_frame ~name:(Temp.newlabel ()) ~formals:[]
  }


let new_level ~parent ~name ~formals_escape =
  { parent = Some parent
  ; frame =
      Frame.new_frame ~name ~formals:(true (* pass static link as an argument *) :: formals_escape)
  }


let formals level = List.map (fun frame_access -> (level, frame_access)) (List.tl (Frame.formals level.frame))

let name level = Frame.name level.frame

let alloc_local level escape = (level, Frame.alloc_local level.frame escape)


type expr = unit [@@deriving show]