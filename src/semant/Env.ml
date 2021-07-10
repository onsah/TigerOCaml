type ty = Types.ty [@@deriving show]

type envEntry =
  | VarEntry of ty
  | FunEntry of
      { argTypes : ty list
      ; return_type : ty
      }
[@@deriving show]

let baseTypes =
  [ (Symbol.symbol "int", Types.Int)
  ; (Symbol.symbol "string", Types.String)
  ; (Symbol.symbol "unit", Types.Unit)
  ; (* TODO: remove this used for testing*)
    ( Symbol.symbol "foo"
    , Types.Record
        ([ { field_id = Symbol.symbol "bar"; field_ty = Types.Int } ], ref ())
    )
  ]


let baseTypeEnv = Symbol.enter_all (Symbol.empty, baseTypes)

let base_values =
  [ ( Symbol.symbol "print"
    , FunEntry { argTypes = [ Types.String ]; return_type = Types.Unit } )
  ; (Symbol.symbol "flush", FunEntry { argTypes = []; return_type = Types.Unit })
  ; ( Symbol.symbol "getchar"
    , FunEntry { argTypes = []; return_type = Types.String } )
  ; ( Symbol.symbol "ord"
    , FunEntry { argTypes = [ Types.String ]; return_type = Types.Int } )
  ; ( Symbol.symbol "chr"
    , FunEntry { argTypes = [ Types.Int ]; return_type = Types.String } )
  ; ( Symbol.symbol "size"
    , FunEntry { argTypes = [ Types.String ]; return_type = Types.Int } )
  ; ( Symbol.symbol "substring"
    , FunEntry
        { argTypes = [ Types.String; Types.Int; Types.Int ]
        ; return_type = Types.String
        } )
  ; ( Symbol.symbol "concat"
    , FunEntry
        { argTypes = [ Types.String; Types.String ]
        ; return_type = Types.String
        } )
  ; ( Symbol.symbol "not"
    , FunEntry { argTypes = [ Types.Int ]; return_type = Types.Int } )
  ; ( Symbol.symbol "exit"
    , FunEntry { argTypes = [ Types.Int ]; return_type = Types.Unit } )
  ]


let baseValueEnv = Symbol.enter_all (Symbol.empty, base_values)
