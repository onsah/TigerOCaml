type level [@@deriving show]

type access [@@deriving show]

type expr = unit [@@deriving show]

val outermost : level

val newLevel : parent:level -> name:Temp.label -> formals:access list -> level

val formals : level -> access list

val allocLocal : level -> bool -> access