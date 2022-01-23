module Frame : Frame.Frame = Frame.MipsFrame

module IRTree = Ir.IRTree

type access = level * Frame.access [@@deriving show]

and level =
  { parent : level option
  ; frame : Frame.frame
  }
[@@deriving show]

let outermost =
  { parent = None
  ; frame = Frame.new_frame ~name:(Temp.newlabel ()) ~formals:[]
  }


let new_level ~parent ~name ~formals_escape =
  { parent = Some parent
  ; frame =
      Frame.new_frame
        ~name
        ~formals:(true (* pass static link as an argument *) :: formals_escape)
  }


let formals level =
  List.map
    (fun frame_access -> (level, frame_access))
    (List.tl (Frame.formals level.frame))


let name level = Frame.name level.frame

let alloc_local level escape = (level, Frame.alloc_local level.frame escape)

type cond_args =
  { true_label : IRTree.label
  ; false_label : IRTree.label
  }

type expr =
  | Expr of IRTree.expr
  | NoValue of IRTree.stmt
  | Cond of (cond_args -> IRTree.stmt)
[@@deriving show]

type typed_expr =
  { expr : expr
  ; ty : Types.ty
  }
[@@deriving show]

let extract_expr = function
  | Expr e ->
      e
  | NoValue stmt ->
      IRTree.ESeq (stmt, IRTree.Const 0)
  | Cond cond_fn ->
      let true_label = Temp.newlabel ()
      and false_label = Temp.newlabel ()
      and result = Temp.newtemp () in
      IRTree.ESeq
        ( Seq
            [ IRTree.Move
                { location = IRTree.Temp result; value = IRTree.const_true }
            ; cond_fn { true_label; false_label }
            ; IRTree.Label false_label
            ; IRTree.Move
                { location = IRTree.Temp result; value = IRTree.const_false }
            ; IRTree.Label true_label
            ]
        , IRTree.Temp result )


let extract_no_result = function
  | Expr e ->
      IRTree.Expr e
  | NoValue stmt ->
      stmt
  | Cond cond_fn ->
      (* Give dummy labels *)
      let dummy_label = Temp.newlabel () in
      Seq
        [ cond_fn { true_label = dummy_label; false_label = dummy_label }
        ; Label dummy_label
        ]


(** TODO :test *)
let rec assert_can_be_int_expr = function
  | IRTree.Const _ | IRTree.Temp _ | IRTree.Mem _ ->
      ()
  | IRTree.Name _ ->
      raise (Failure "label can't be used as a condition expression")
  | IRTree.Binop { left; right; _ } ->
      assert_can_be_int_expr left ;
      assert_can_be_int_expr right ;
      ()
  | IRTree.Call _ ->
      raise (Failure "call can't be condition expression")
  | IRTree.ESeq (_, expr) ->
      assert_can_be_int_expr expr


(* TODO: create is_int_expr, use it to validate expr type *)
let extract_cond = function
  | Expr e ->
    ( match e with
    | Const i when IRTree.is_falsy i ->
        fun { false_label; _ } ->
          IRTree.Jump
            { expr = IRTree.Name false_label; labels = [ false_label ] }
    | Const i when IRTree.is_truthy i ->
        fun { true_label; _ } ->
          IRTree.Jump { expr = IRTree.Name true_label; labels = [ true_label ] }
    | _ ->
        assert_can_be_int_expr e ;
        fun { true_label; false_label } ->
          IRTree.CondJump
            { cond = IRTree.Eq
            ; left_expr = e
            ; right_expr = IRTree.Const 1
            ; true_label
            ; false_label
            } )
  | NoValue _ ->
      raise (Failure "Can't convert no value to conditional")
  | Cond cond_fn ->
      cond_fn


let dummy_expr = NoValue (IRTree.Expr (IRTree.Const 0))

let static_link frame = List.hd (Frame.formals frame)

(* Can be used for prımitive values as well as record and array values *)
let simple_var ((access_level, frame_access), level) =
  let rec simple_var_impl (curr_level, prev_expr) =
    if curr_level == access_level
    then Expr (Frame.expr frame_access ~fp:prev_expr)
    else
      let next_expr = Frame.expr (static_link curr_level.frame) ~fp:prev_expr in
      match curr_level.parent with
      | None ->
          raise (Failure "Expected to have parent")
      | Some parent ->
          simple_var_impl (parent, next_expr)
  in

  simple_var_impl (level, IRTree.Temp Frame.fp)


(* construct ir exor and continue *)

(* Can be used both for arrray subscripts and record field accesses *)
(* TODO: OOB check *)
let subscript (array_access, level, offset_expr) =
  let array_start = simple_var (array_access, level) in
  Expr
    (Mem
       (Binop
          { op = Plus
          ; left = extract_expr array_start
          ; right =
              Binop
                { op = Mul
                ; left = Const Frame.word_size
                ; right = extract_expr offset_expr
                }
          } ) )


let jump_to_label label =
  IRTree.Jump { expr = IRTree.Name label; labels = [ label ] }


(* TODO: if either than or else is conditional expression, optimize according to page 162 *)
let if_else (cond_expr, then_expr, else_expr) =
  let regular_if cond then' else' =
    let true_label = Temp.newlabel ()
    and false_label = Temp.newlabel ()
    and body_result = Temp.newtemp ()
    (*** After branch ends *)
    and join_label = Temp.newlabel () in
    Expr
      (IRTree.ESeq
         ( Seq
             [ cond { true_label; false_label }
             ; IRTree.Label true_label
             ; IRTree.Move { location = IRTree.Temp body_result; value = then' }
             ; jump_to_label join_label
             ; IRTree.Label false_label
             ; IRTree.Move { location = IRTree.Temp body_result; value = else' }
               (* No need to jump to join if branch is false *)
             ; IRTree.Label join_label
             ]
         , IRTree.Temp body_result ) )
  (* if optimized for and expressions *)
  and and_if cond then_cond =
    let cond_true_label = Temp.newlabel ()
    and then_true_label = Temp.newlabel ()
    and common_false_label = Temp.newlabel ()
    and result = Temp.newtemp ()
    and join_label = Temp.newlabel () in
    Expr
      (IRTree.ESeq
         ( Seq
             [ cond
                 { true_label = cond_true_label
                 ; false_label = common_false_label
                 }
             ; IRTree.Label cond_true_label
             ; then_cond
                 { true_label = then_true_label
                 ; false_label = common_false_label
                 }
             ; IRTree.Label then_true_label
             ; IRTree.Move
                 { location = IRTree.Temp result; value = IRTree.const_true }
             ; jump_to_label join_label
             ; IRTree.Label common_false_label
             ; IRTree.Move
                 { location = IRTree.Temp result; value = IRTree.const_false }
             ; IRTree.Label join_label
             ]
         , IRTree.Temp result ) )
  (* if optimized for or expressions*)
  and or_if cond else_cond =
    let cond_false_label = Temp.newlabel ()
    and else_false_label = Temp.newlabel ()
    and common_true_label = Temp.newlabel ()
    and result = Temp.newtemp ()
    and join_label = Temp.newlabel () in
    Expr
      (IRTree.ESeq
         ( Seq
             [ cond
                 { true_label = common_true_label
                 ; false_label = cond_false_label
                 }
             ; IRTree.Label cond_false_label
             ; else_cond
                 { true_label = common_true_label
                 ; false_label = else_false_label
                 }
             ; IRTree.Label else_false_label
             ; IRTree.Move
                 { location = IRTree.Temp result; value = IRTree.const_false }
             ; jump_to_label join_label
             ; IRTree.Label common_true_label
             ; IRTree.Move
                 { location = IRTree.Temp result; value = IRTree.const_true }
             ; IRTree.Label join_label
             ]
         , IRTree.Temp result ) )
  in
  let cond = extract_cond cond_expr in
  match (then_expr, else_expr) with
  | Cond then_cond, Expr else_expr when IRTree.is_int_and_falsy else_expr ->
      (* And operator *)
      and_if cond then_cond
  | Expr then_expr, Cond else_cond when IRTree.is_int_and_truthy then_expr ->
      (* Else operator *)
      or_if cond else_cond
  | _, _ ->
      let then' = extract_expr then_expr
      and else' = extract_expr else_expr in
      regular_if cond then' else'


let int i = Expr (IRTree.Const i)

let syntax_binop_to_relop binop =
  match binop with
  | Syntax.BinaryEq ->
      Ok IRTree.Eq
  | Syntax.BinaryLtgt ->
      Ok IRTree.Ne
  | Syntax.BinaryLt ->
      Ok IRTree.Lt
  | Syntax.BinaryGt ->
      Ok IRTree.Gt
  | Syntax.BinaryLteq ->
      Ok IRTree.Le
  | Syntax.BinaryGteq ->
      Ok IRTree.Ge
  | _ ->
      Error
        (invalid_arg
           (Printf.sprintf
              "Can't convert %s to IRTree.relop"
              (Syntax.show_binary_op binop) ) )


let comparison
    ( { expr = left_expr; ty = left_ty }
    , binop
    , { expr = right_expr; ty = right_ty } ) =
  let left_expr = extract_expr left_expr
  and right_expr = extract_expr right_expr
  and relop =
    match syntax_binop_to_relop binop with
    | Ok relop ->
        relop
    | Error exn ->
        raise exn
  in
  match (left_ty, right_ty) with
  | Types.String, Types.String ->
    ( match (left_expr, right_expr) with
    | IRTree.Name _, IRTree.Name _ ->
        Expr
          (IRTree.Call
             { func = IRTree.Name IRTree.BuiltIns.string_equal
             ; args = [ left_expr; right_expr ]
             } )
    | _ ->
        raise
        @@ Invalid_argument
             (Printf.sprintf
                "Expected name ir expr for string comparison, got: left=%s, \
                 right=%s"
                (IRTree.show_expr left_expr)
                (IRTree.show_expr right_expr) ) )
  | _ when left_ty = right_ty ->
      Cond
        (fun { true_label; false_label } ->
          IRTree.CondJump
            { cond = relop; left_expr; right_expr; true_label; false_label } )
  | _ ->
      raise
      @@ Invalid_argument
           (Printf.sprintf
              "Can't compare, types are different: left=%s, right=%s"
              (Types.show_ty left_ty)
              (Types.show_ty right_ty) )
