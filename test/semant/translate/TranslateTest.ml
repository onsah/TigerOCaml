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
  and cond = IRTree.Temp (Temp.newtemp ())
  and break_label = Temp.newlabel () in
  let result =
    Translate.while' ~cond:(Expr cond) ~body:(Expr body) ~break_label
  in
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
          ~msg:"done label must be the break label given"
          while_done
          break_label ;
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
  assert_equal result (NoValue (IRTree.jump_single_label label))


let test_for _ =
  let from, to' = (0, 5)
  and loop_var_location_expr = IRTree.Temp (Temp.newtemp ())
  and body_expr = Translate.int 0 in
  let result =
    Translate.for'
      ~var:(Translate.Expr loop_var_location_expr)
      ~from:(Translate.int from)
      ~to':(Translate.int to')
      ~body:body_expr
      ~break_label:(Temp.newlabel ())
  in
  ignore
    ( match result with
    | Translate.Expr
        (IRTree.ESeq
          ( IRTree.Seq
              [ IRTree.Move
                  { location = loop_var_location_expr'
                  ; value = loop_var_init_value
                  }
              ; IRTree.Move
                  { location = limit_location_expr; value = limit_value }
              ; IRTree.CondJump
                  { true_label = initial_check_true_label
                  ; false_label = initial_check_false_label
                  ; left_expr = initial_check_left_expr
                  ; right_expr = initial_check_right_expr
                  ; cond = initial_test_cond
                  }
              ; IRTree.Label loop_start_label
              ; IRTree.Expr body_expr'
              ; IRTree.CondJump
                  { true_label = next_iter_check_true_label
                  ; false_label = next_iter_check_false_label
                  ; left_expr = next_iter_check_left_expr
                  ; right_expr = next_iter_check_right_expr
                  ; cond = next_iter_test_cond
                  }
              ; IRTree.Label loop_cont_label
              ; IRTree.Move
                  { location = loop_incr_location; value = loop_incr_value }
              ; loop_cont_jump
              ; IRTree.Label break_label
              ]
          , for_result_expr ) ) ->
        assert_equal loop_var_location_expr' loop_var_location_expr ;
        assert_equal
          loop_var_init_value
          (Translate.extract_expr (Translate.int from)) ;
        assert_equal limit_value (Translate.extract_expr (Translate.int to')) ;
        assert_equal initial_check_true_label break_label ;
        assert_equal initial_check_false_label loop_start_label ;
        assert_equal initial_check_left_expr loop_var_location_expr ;
        assert_equal initial_check_right_expr limit_location_expr ;
        assert_equal initial_test_cond IRTree.Gt ;
        assert_equal body_expr' (Translate.extract_expr body_expr) ;
        assert_equal next_iter_check_true_label loop_cont_label ;
        assert_equal next_iter_check_false_label break_label ;
        assert_equal next_iter_check_left_expr loop_var_location_expr ;
        assert_equal next_iter_check_right_expr limit_location_expr ;
        assert_equal next_iter_test_cond IRTree.Lt ;
        assert_equal loop_incr_location loop_var_location_expr ;
        assert_equal
          loop_incr_value
          (IRTree.Binop
             { op = IRTree.Plus
             ; left = loop_var_location_expr
             ; right = IRTree.Const 1
             } ) ;
        assert_equal loop_cont_jump (IRTree.jump_single_label loop_start_label) ;
        assert_equal for_result_expr IRTree.const_unit
    | _ ->
        assert_bool "didn't match pattern" false )


let test_function_call_regular _ =
  let label = Temp.newlabel ()
  and args_passed = []
  and caller_level = Translate.outermost in
  let callee_level =
    Translate.new_level
      ~parent:caller_level
      ~name:label
      ~formals_escape:[ true ]
  in
  let result =
    Translate.function_call ~label ~args:args_passed ~callee_level ~caller_level
  in
  ignore
    ( match result with
    | Translate.Expr (IRTree.Call { func; args }) ->
        assert_equal func (IRTree.Name label) ;
        assert_equal args (IRTree.Temp Frame.MipsFrame.fp :: args_passed)
    | _ ->
        assert_bool "didn't match pattern" false )


let test_function_call_recursive _ =
  let label = Temp.newlabel ()
  and args_passed = [] in
  let caller_level =
    Translate.new_level
      ~parent:Translate.outermost
      ~name:label
      ~formals_escape:[ true ]
  in
  let callee_level = caller_level in
  let result =
    Translate.function_call ~label ~args:args_passed ~callee_level ~caller_level
  in
  ignore
    ( match result with
    | Translate.Expr (IRTree.Call { func; args = static_link_formal :: args })
      ->
        assert_equal func (IRTree.Name label) ;
        assert_equal args args_passed ;
        assert_equal
          static_link_formal
          (IRTree.Mem
             (IRTree.Binop
                { left = IRTree.Const 0
                ; right = IRTree.Temp Frame.MipsFrame.fp
                ; op = IRTree.Plus
                } ) )
    | _ ->
        assert_bool "didn't match pattern" false )


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
       ; "test_for" >:: test_for
       ; "test_function_call_regular" >:: test_function_call_regular
       ; "test_function_call_recursive" >:: test_function_call_recursive
       ]


let () = run_test_tt_main suite
