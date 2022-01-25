type temp = int [@@deriving show]

let temps = ref 100

let newtemp () =
  let t = !temps in
  temps := t + 1 ;
  t


let makestring temp = Printf.sprintf "t %s" (string_of_int temp)

let labels_counter = ref 0

type label = Symbol.symbol [@@deriving show]

let newlabel () : label =
  let i = !labels_counter in
  labels_counter := i + 1 ;
  let name = Printf.sprintf "L%d" i in
  Symbol.symbol name


let namedlabel name : label = Symbol.symbol name
