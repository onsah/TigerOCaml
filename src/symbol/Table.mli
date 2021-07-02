module type GetInt = sig
    type t 
    val getInt: t -> int
end

module type TableT = sig

    type key
    type 't table

    val empty: 't table 
    val enter: 't table * key * 't -> 't table
    val look: 't table * key -> 't option
end

module IntMapTable(T: GetInt): TableT with type key = T.t