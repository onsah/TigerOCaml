type symbol
val pp_symbol : Format.formatter -> symbol -> unit
val show_symbol : symbol -> string
val symbol : string -> symbol
val name : symbol -> string

type 't table
val empty: 't table
val enter: 't table * symbol * 't -> 't table
val look: 't table * symbol -> 't option