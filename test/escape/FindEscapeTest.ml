open OUnit2
open FindEscape
open Syntax
open TestUtils

let dummy_pos : Syntax.pos = { line = -1; col = -1 }

let suite =
  "TestSuit1"
  >::: [ ( "test escape let var"
         >:: fun _ ->
         let x_escape = ref false in
         let expr =
           Syntax.Expr
             { pos = dummy_pos
             ; expr =
                 LetExpr
                   { decls =
                       [ VarDecl
                           { name = Symbol.symbol "x"
                           ; typ =
                               Some
                                 (Type
                                    { pos = dummy_pos
                                    ; symbol = Symbol.symbol "int"
                                    } )
                           ; escape = x_escape
                           ; value = Utils.int_expr 3
                           ; pos = dummy_pos
                           }
                       ; FunctionDecls
                           [ FunDecl
                               { name = Symbol.symbol "foo"
                               ; params = []
                               ; return_type =
                                   Some
                                     (Type
                                        { pos = dummy_pos
                                        ; symbol = Symbol.symbol "int"
                                        } )
                               ; pos = dummy_pos
                               ; body =
                                   Expr
                                     { pos = dummy_pos
                                     ; expr =
                                         LValueExpr
                                           { lvalue =
                                               SimpleVar
                                                 { pos = dummy_pos
                                                 ; symbol = Symbol.symbol "x"
                                                 }
                                           }
                                     }
                               }
                           ]
                       ]
                   ; body = Expr { pos = dummy_pos; expr = SeqExpr [] }
                   }
             }
         in
         let _ = find_escape expr in
         assert_bool "x must escape" !x_escape )
       ; ( "test escape for var"
         >:: fun _ ->
         let for_var_escape = ref false in
         let expr =
           Expr
             { pos = dummy_pos
             ; expr =
                 ForExpr
                   { var = Symbol.symbol "i"
                   ; escape = for_var_escape
                   ; from = Utils.int_expr 0
                   ; to' = Utils.int_expr 1
                   ; body =
                       Utils.expr
                         (LetExpr
                            { decls =
                                [ FunctionDecls
                                    [ FunDecl
                                        { name = Symbol.symbol "foo"
                                        ; params = []
                                        ; return_type = None
                                        ; body = Utils.simple_var "i"
                                        ; pos = dummy_pos
                                        }
                                    ]
                                ]
                            ; body = Utils.unit
                            } )
                   }
             }
         in
         let _ = find_escape expr in
         assert_bool "for variable 'i' must escape" !for_var_escape )
       ; ( "test escape function formal parameter"
         >:: fun _ ->
         let func_param_escape = ref false in
         let expr =
           Utils.expr
             (LetExpr
                { decls =
                    [ FunctionDecls
                        [ FunDecl
                            { name = Symbol.symbol "foo"
                            ; params =
                                [ TypedField
                                    { name = Symbol.symbol "x"
                                    ; typ = Symbol.symbol "int"
                                    ; pos = dummy_pos
                                    ; escape = func_param_escape
                                    }
                                ]
                            ; return_type = None
                            ; pos = dummy_pos
                            ; body =
                                Utils.expr
                                  (LetExpr
                                     { decls =
                                         [ FunctionDecls
                                             [ FunDecl
                                                 { name = Symbol.symbol "bar"
                                                 ; params = []
                                                 ; return_type = None
                                                 ; pos = dummy_pos
                                                 ; body = Utils.simple_var "x"
                                                 }
                                             ]
                                         ]
                                     ; body = Utils.unit
                                     } )
                            }
                        ]
                    ]
                ; body = Utils.unit
                } )
         in
         let _ = find_escape expr in
         assert_bool "function param 'x' must escape" !func_param_escape )
       ; ( "test no escape"
         >:: fun _ ->
         let x_escape = ref false in
         let expr =
           Utils.expr
             (LetExpr
                { decls =
                    [ Utils.decl_var ~escape:x_escape "x"
                    ; Utils.decl_var "y"
                    ; FunctionDecls
                        [ FunDecl
                            { name = Symbol.symbol "foo"
                            ; params = []
                            ; return_type = None
                            ; pos = dummy_pos
                            ; body =
                                Utils.simple_var "y"
                                (* use the other variable *)
                            }
                        ]
                    ]
                ; body = Utils.unit
                } )
         in
         let _ = find_escape expr in
         (* X shouldn't be escaped *)
         assert_bool "value must not escape" (not !x_escape) )
       ]


let () = run_test_tt_main suite
