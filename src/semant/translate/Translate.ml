open Utils

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
    | Const 0 ->
        fun { false_label; _ } ->
          IRTree.Jump
            { expr = IRTree.Name false_label; labels = [ false_label ] }
    | Const _ ->
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


let dummy_expr = __ ()
