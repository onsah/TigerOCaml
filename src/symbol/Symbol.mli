type symbol [@@deriving show]

val symbol : string -> symbol

val name : symbol -> string

type 't table

val empty : 't table

val enter : 't table * symbol * 't -> 't table

val enter_all : 't table * (symbol * 't) list -> 't table

val look : 't table * symbol -> 't option
