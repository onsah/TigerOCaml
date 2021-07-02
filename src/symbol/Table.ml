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

module IntMapTable(T: GetInt) = struct
  
  type key = T.t

  module TOrdType = struct
    type t = T.t
    let compare t1 t2 = Int.compare (T.getInt t1) (T.getInt t2)
  end

  module Table = Map.Make(TOrdType) 
  type 't table = 't Table.t

  let empty = Table.empty

  let enter (t, k, v) = Table.add k v t
  let look(t, k) = Table.find_opt k t 
end