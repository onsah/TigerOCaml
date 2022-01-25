type temp [@@deriving show]

val newtemp : unit -> temp

val makestring : temp -> string

type label = Symbol.symbol [@@deriving show]

val newlabel : unit -> label

val namedlabel : string -> label
