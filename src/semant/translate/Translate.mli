type level [@@deriving show]

type access [@@deriving show]

type expr = unit [@@deriving show]

val outermost : level

val new_level : parent:level -> name:Temp.label -> formals_escape:bool list -> level

val formals : level -> access list

val name : level -> Temp.label

val alloc_local : level -> bool -> access