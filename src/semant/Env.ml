type ty = Types.ty [@@deriving show]

type envEntry =
  | VarEntry of
      { access : Translate.access
      ; ty : ty
      }
  | FunEntry of
      { level : Translate.level
      ; label : Temp.label
      ; argTypes : ty list
      ; return_type : ty
      }
[@@deriving show]

let baseTypes =
  [ (Symbol.symbol "int", Types.Int)
  ; (Symbol.symbol "string", Types.String)
  ; (Symbol.symbol "unit", Types.Unit)
  ]


let baseTypeEnv = Symbol.enter_all (Symbol.empty, baseTypes)

let base_values =
  List.map
    (fun (name, (argTypes, retType)) ->
      ( Symbol.symbol name
      , FunEntry
          { argTypes
          ; return_type = retType
          ; label = Temp.namedlabel name
          ; level = Translate.outermost
          } ) )
    [ ("print", ([ Types.String ], Types.Unit))
    ; ("flush", ([], Types.Unit))
    ; ("getchar", ([], Types.String))
    ; ("ord", ([ Types.String ], Types.Int))
    ; ("chr", ([ Types.Int ], Types.String))
    ; ("size", ([ Types.String ], Types.Int))
    ; ("substring", ([ Types.String; Types.Int; Types.Int ], Types.String))
    ; ("concat", ([ Types.String; Types.String ], Types.String))
    ; ("not", ([ Types.Int ], Types.Int))
    ; ("exit", ([ Types.Int ], Types.Unit))
    ]


let baseValueEnv = Symbol.enter_all (Symbol.empty, base_values)
