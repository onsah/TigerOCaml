open OUnit2
open Ir

(*
This module contains end to end code generation tests
*)

let test_break_outside_while _ =
  assert_raises
    (TigerError.TigerError.make_semant_error
       ( "No surrouding break label for break statement. Break appears to be \
          outside of a while expression."
       , { line = 1; col = 1 } ) )
    (fun _ ->
      Main.analyze_file ~path:"../scripts/translate/break_outside_while.tig" )


let test_break_inside_while _ =
  let expr =
    Result.get_ok
    @@ Main.analyze_file ~path:"../scripts/translate/break_inside_while.tig"
  in
  ignore (Printf.printf "%s\n" (Translated.show_typedExpr expr)) ;
  ignore
    ( match expr.translated_expr.translated_expr with
    | Translate.Expr
        (IRTree.ESeq
          ( IRTree.Seq
              [ IRTree.Label _
              ; IRTree.CondJump _
              ; IRTree.Label _
              ; IRTree.Expr
                  (IRTree.ESeq
                    (IRTree.Jump { labels = [ while_exit_label' ]; _ }, _) )
              ; IRTree.Jump _
              ; IRTree.Label while_exit_label
              ]
          , _ ) )
      when while_exit_label' = while_exit_label ->
        assert_bool "success" true
    | _ ->
        assert_bool
          (Printf.sprintf
             "match failed, tried to match: %s"
             (Translated.show_typedExpr expr) )
          false )


let test_let _ =
  let expr =
    Result.get_ok @@ Main.analyze_file ~path:"../scripts/translate/let.tig"
  in
  ignore
    ( match expr.translated_expr.translated_expr with
    | Translate.Expr
        (IRTree.ESeq
          ( IRTree.Seq [ IRTree.Move { location = var_location; value } ]
          , var_location' ) ) ->
        assert_equal var_location var_location' ;
        assert_equal value (Const 13)
    | _ ->
        assert_bool
          (Printf.sprintf
             "match failed, tried to match: %s"
             (Translated.show_typedExpr expr) )
          false )


let suite =
  "SemantTest"
  >::: [ "test_break_outside_while" >:: test_break_outside_while
       ; "test_break_inside_while" >:: test_break_inside_while
       ; "test_let" >:: test_let
       ]


let () = run_test_tt_main suite
