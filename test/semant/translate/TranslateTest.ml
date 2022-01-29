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


let test_record _ =
  let fields =
    [ Translate.Expr (IRTree.Const 1)
    ; Translate.Expr (IRTree.Const 2)
    ; Translate.Expr (IRTree.Const 3)
    ]
  in
  let result = Translate.record ~fields in
  ignore
    ( match result with
    | Expr
        (IRTree.ESeq
          ( IRTree.Seq
              [ IRTree.Move
                  { location = IRTree.Temp r'
                  ; value =
                      IRTree.Call
                        { args = [ IRTree.Const 12 ]; func = IRTree.Name _ }
                  }
              ; IRTree.Move
                  { location =
                      IRTree.Binop
                        { op = IRTree.Plus
                        ; left = IRTree.Temp r''
                        ; right = IRTree.Const 0
                        }
                  ; value = e'
                  }
              ; IRTree.Move
                  { location =
                      IRTree.Binop
                        { op = IRTree.Plus
                        ; left = IRTree.Temp r'''
                        ; right = IRTree.Const 4
                        }
                  ; value = e''
                  }
              ; IRTree.Move
                  { location =
                      IRTree.Binop
                        { op = IRTree.Plus
                        ; left = IRTree.Temp r''''
                        ; right = IRTree.Const 8
                        }
                  ; value = e'''
                  }
              ]
          , IRTree.Temp r ) )
      when r = r'
           && r = r''
           && r = r'''
           && r = r''''
           && fields = List.map (fun e -> Translate.Expr e) [ e'; e''; e''' ] ->
        assert_bool "matched pattern" true
    | _ ->
        assert_bool "didn't match pattern" false )


let test_while _ =
  let body = IRTree.Temp (Temp.newtemp ())
  and cond = IRTree.Temp (Temp.newtemp ()) in
  let result = Translate.while' ~cond:(Expr cond) ~body:(Expr body) in
  ignore
    ( match result with
    | Expr
        (IRTree.ESeq
          ( IRTree.Seq
              [ IRTree.Label test
              ; IRTree.CondJump
                  { cond = IRTree.Eq
                  ; left_expr = cond_expr
                  ; right_expr = true_expr
                  ; true_label = true_label'
                  ; false_label = false_label'
                  }
              ; IRTree.Label cont_label
              ; IRTree.Expr body'
              ; IRTree.Jump { labels = [ test' ]; _ }
              ; IRTree.Label while_done
              ]
          , dummy_value ) ) ->
        assert_equal
          ~msg:
            "condition true label must be continue label to keep while loop \
             iterating"
          true_label'
          cont_label ;
        assert_equal
          ~msg:"condition false label must be the label exiting while loop"
          false_label'
          while_done ;
        assert_equal
          ~msg:"condition must be condition expression given"
          cond_expr
          cond ;
        assert_equal
          ~msg:"true expression of the condition must be const_true"
          true_expr
          IRTree.const_true ;
        assert_equal ~msg:"body must be body expression given" body' body ;
        assert_equal
          ~msg:"while end should jump back to the test label"
          test'
          test ;
        assert_equal
          ~msg:"while result expression should be const_unit"
          dummy_value
          IRTree.const_unit
    | _ ->
        assert_bool "didn't match pattern" false )


let test_array _ =
  let size = 4
  and init_expr = Translate.Expr (IRTree.Const 77) in
  let result = Translate.array ~size ~init_expr in
  ignore
    ( match result with
    | Translate.Expr
        (IRTree.ESeq
          ( IRTree.Seq
              [ (* malloc *)
              IRTree.Move
                { location = IRTree.Temp t'
                ; value = IRTree.Call { args = [ IRTree.Const 16 ]; _ }
                }
              ; (* initialize the values *)
                IRTree.Expr
                  (IRTree.Call { args = [ IRTree.Const 4; init_expr' ]; _ })
              ]
          , IRTree.Temp t ) )
      when t = t' && init_expr = Expr init_expr' ->
        assert_bool "matched pattern" true
    | _ ->
        assert_bool "didn't match pattern" false )


let test_break _ =
  let label = Temp.newlabel () in
  let result = Translate.break' label in
  assert_equal result (NoValue (IRTree.Label label))


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
       ; "test_record" >:: test_record
       ; "test_array" >:: test_array
       ; "test_while" >:: test_while
       ; "test_break" >:: test_break
       ]


let () = run_test_tt_main suite
