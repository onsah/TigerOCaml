open OUnit2
open Ir

let test_regular_if _ =
  let expr =
    Result.get_ok
      (Main.analyze_file ~path:"../../scripts/translate/regular_if.tig")
  in
  ignore (assert_equal expr.ty Types.String) ;
  ignore
    ( match expr.translated_expr.translated_expr with
    | Translate.Expr
        (IRTree.ESeq
          ( IRTree.Seq
              [ IRTree.CondJump { true_label; false_label; _ }
              ; IRTree.Label true_label'
              ; IRTree.Move { location = IRTree.Temp result; _ }
              ; IRTree.Jump { labels = [ join_label ]; _ }
              ; IRTree.Label false_label'
              ; IRTree.Move { location = IRTree.Temp result'; _ }
              ; IRTree.Label join_label'
              ]
          , IRTree.Temp result'' ) )
      when true_label = true_label'
           && false_label = false_label'
           && join_label = join_label'
           && result = result'
           && result' = result'' ->
        assert_bool "matched pattern" true
    | _ ->
        assert_bool
          (Core.sprintf
             "didn't match pattern: %s"
             (Translated.show_typedExpr expr) )
          false )


let test_conitional_and_if _ =
  let expr =
    Result.get_ok
      (Main.analyze_file ~path:"../../scripts/translate/conditional_and_if.tig")
  in
  ignore (assert_equal expr.ty Types.Int) ;
  ignore
    ( match expr.translated_expr.translated_expr with
    | Expr
        (ESeq
          ( Seq
              [ IRTree.CondJump
                  { true_label = true_label_1'; false_label = false_label'; _ }
              ; IRTree.Label true_label_1
              ; IRTree.CondJump
                  { true_label = true_label_2'; false_label = false_label''; _ }
              ; IRTree.Label true_label_2
              ; IRTree.Move { location = IRTree.Temp result'; value = Const 1 }
              ; IRTree.Jump { labels = [ join_label' ]; _ }
              ; IRTree.Label false_label
              ; IRTree.Move { location = IRTree.Temp result''; value = Const 0 }
              ; IRTree.Label join_label
              ]
          , IRTree.Temp result ) )
      when true_label_1' = true_label_1
           && false_label' = false_label
           && true_label_2' = true_label_2
           && false_label'' = false_label
           && join_label' = join_label
           && result' = result
           && result'' = result ->
        assert_bool "matched pattern" true
    | _ ->
        assert_bool
          (Core.sprintf
             "didn't match pattern: %s"
             (Translated.show_typedExpr expr) )
          false )


let suite =
  "Translate"
  >::: [ "if_else should process if expressions normally" >:: test_regular_if
       ; "if_else should optimize if expressions with comparions"
         >:: test_conitional_and_if
       ]


let _ = run_test_tt_main suite
