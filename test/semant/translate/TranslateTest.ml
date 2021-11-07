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


let test_simple_var_accessing_own_variable _ =
  let local = Translate.alloc_local Translate.outermost true in
  let var_expr = Translate.simple_var (local, Translate.outermost) in
  assert_equal
    var_expr
    (Expr
       (IRTree.Mem
          (IRTree.Binop
             { op = IRTree.Plus
             ; left = IRTree.Const (-4) (* Becase 0 offset is static link *)
             ; right = IRTree.Temp Frame.MipsFrame.fp
             } ) ) )


let test_simple_var_accessing_parent_variable _ =
  let level1 =
    Translate.new_level
      ~parent:Translate.outermost
      ~name:(Temp.newlabel ())
      ~formals_escape:[]
  in
  let level2 =
    Translate.new_level
      ~parent:level1
      ~name:(Temp.newlabel ())
      ~formals_escape:[]
  in
  (* allocate at top level *)
  let local = Translate.alloc_local Translate.outermost true in
  let var_expr = Translate.simple_var (local, level2) in
  let _ = Printf.printf "%s\n" (Translate.show_expr var_expr) in
  assert_equal
    var_expr
    (Expr
       (IRTree.Mem
          (IRTree.Binop
             { op = IRTree.Plus
             ; left = IRTree.Const (-4)
             ; right =
                 IRTree.Mem
                   (IRTree.Binop
                      { op = IRTree.Plus
                      ; left = IRTree.Const 0
                      ; right =
                          IRTree.Mem
                            (IRTree.Binop
                               { op = IRTree.Plus
                               ; left = IRTree.Const 0
                               ; right = IRTree.Temp Frame.MipsFrame.fp
                               } )
                      } )
             } ) ) )


let suite =
  "Translate"
  >::: [ "extract_cond should return jump false label for const 0"
         >:: test_extranct_cond_const_0
       ; "extract_cond should return jump true label for const 1"
         >:: test_extranct_cond_const_1
       ; "test_simple_var_accessing_own_variable"
         >:: test_simple_var_accessing_own_variable
       ; "test_simple_var_accessing_parent_variable"
         >:: test_simple_var_accessing_parent_variable
       ]


let () = run_test_tt_main suite
