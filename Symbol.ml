type symbol = Symbol of string
[@@deriving show]

let symbol str = Symbol str

let name symbol = match symbol with
  | Symbol str -> str