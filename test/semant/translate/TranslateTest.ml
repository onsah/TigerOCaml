open OUnit2
open Ir

let test_true_label = Temp.namedlabel "test_true_label"

let test_false_label = Temp.namedlabel "test_false_label"

let test_cond_args : Translate.cond_args =
  { true_label = test_true_label; false_label = test_false_label }


let test_extranct_cond_const_0 _ =
  let expr = IRTree.Const 0 in
  let test_cond = Translate.extract_cond (Expr expr) in
  let result = test_cond test_cond_args in
  assert_equal
    result
    (Jump { expr = IRTree.Name test_false_label; labels = [ test_false_label ] })


let test_extranct_cond_const_1 _ =
  let expr = IRTree.Const 1 in
  let test_cond = Translate.extract_cond (Expr expr) in
  let result = test_cond test_cond_args in
  assert_equal
    result
    (Jump { expr = IRTree.Name test_true_label; labels = [ test_true_label ] })


let suite =
  "Translate"
  >::: [ "extract_cond should return jump false label for const 0"
         >:: test_extranct_cond_const_0
       ; "extract_cond should return jump true label for const 1"
         >:: test_extranct_cond_const_1
       ]


let () = run_test_tt_main suite
