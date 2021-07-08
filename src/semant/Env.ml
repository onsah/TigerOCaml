type ty = Types.ty [@@deriving show]

type envEntry =
  | VarEntry of ty
  | FunEntry of
      { argTypes : ty list
      ; return_type : ty
      }
[@@deriving show]

let baseTypes =
  [ Symbol.symbol "int", Types.Int
  ; Symbol.symbol "string", Types.String
  ; Symbol.symbol "unit", Types.Unit
  ; (* TODO: remove this used for testing*)
    ( Symbol.symbol "foo"
    , Types.Record ([ { field_id = Symbol.symbol "bar"; field_ty = Types.Int } ], ref ())
    )
  ]
;;

let baseTypeEnv = Symbol.enter_all (Symbol.empty, baseTypes)

let base_values =
  [ ( Symbol.symbol "print"
    , FunEntry { argTypes = [ Types.String ]; return_type = Types.Unit } )
  ; ( Symbol.symbol "band"
    , VarEntry
        (Types.Record
           ([ { field_id = Symbol.symbol "guitarists"; field_ty = Types.Array (Types.String, ref ()) } ], ref ()))
    )
  ;( Symbol.symbol "banddd"
    , VarEntry
        (Types.Record
           ([ { field_id = Symbol.symbol "guitarists"; field_ty = Types.Array (Types.String, ref ()) } ], ref ()))
    )
  ; (Symbol.symbol "language", VarEntry Types.String)
  ; (Symbol.symbol "arr", VarEntry (Types.Array (Types.String, ref ())))
    (* TODO: other built ins *)
  ]
;;

let baseValueEnv = Symbol.enter_all (Symbol.empty, base_values)
