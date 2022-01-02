open OUnit2
open Ir

let test_regular_if _ =
  ignore (Core.printf "path %s\n" (Sys.getcwd ())) ;
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


let suite =
  "Translate"
  >::: [ "if_else should process if expressions normally" >:: test_regular_if ]


let _ = run_test_tt_main suite
