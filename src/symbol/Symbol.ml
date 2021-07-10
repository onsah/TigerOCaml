type symbol = Symbol of (string * int) [@@deriving show]

let nextSymbolId = ref 0

let getNextSymbolId () =
  let next = !nextSymbolId in
  nextSymbolId := !nextSymbolId + 1 ;
  next


let sizeHint = 128

let hashtable : (string, int) Hashtbl.t = Hashtbl.create sizeHint

let symbol str =
  match Hashtbl.find_opt hashtable str with
  | Some id ->
      Symbol (str, id)
  | None ->
      let id = getNextSymbolId () in
      Hashtbl.add hashtable str id ;
      Symbol (str, id)


let name (Symbol (name, _)) = name

module Table = Table.IntMapTable (struct
  type t = symbol

  let getInt (Symbol (_, i)) = i
end)

type 't table = 't Table.table

let empty = Table.empty

let enter = Table.enter

let enter_all (t, entries) =
  List.fold_left (fun prev (key, value) -> enter (prev, key, value)) t entries


let look = Table.look
