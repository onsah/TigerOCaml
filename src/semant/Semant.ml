open TigerError
open Translated
open Syntax
open Types
open Printf

type valueEnv = Env.envEntry Symbol.table
type typeEnv = Env.ty Symbol.table

let expecting_int ty pos =
  match ty with
  | Types.Int -> ()
  | _ -> TigerError.semant_error ("Expecting int", pos)
;;

let rec transExpr (value_env, type_env, Syntax.Expr { expr; pos }) =
  match expr with
  | Syntax.IntExpr _ ->
    { translated_expr = { translated_expr = (); pos }; ty = Types.Int }
  | Syntax.NilExpr -> { translated_expr = { translated_expr = (); pos }; ty = Types.Nil }
  | Syntax.StringExpr _ ->
    { translated_expr = { translated_expr = (); pos }; ty = Types.String }
  | Syntax.CallExpr { func; args } ->
    let entry = check_look_env (value_env, func, pos) in
    (match entry with
    | Env.FunEntry { argTypes; return_type } ->
      let args_checked =
        List.map (fun arg -> transExpr (value_env, type_env, arg)) args
      in
      type_check_args argTypes (List.map (fun arg -> arg.ty) args_checked) pos;
      { translated_expr = { translated_expr = (); pos }; ty = return_type }
    | other ->
      TigerError.semant_error
        ( "Variable "
          ^ Symbol.name func
          ^ " is expected to be function, but found "
          ^ Env.show_envEntry other
        , pos ))
  | Syntax.BinExpr { left; right; op } ->
    transBinary
      (value_env, type_env, Syntax.Expr { expr = Syntax.BinExpr { left; right; op }; pos })
  | Syntax.RecordExpr { typ; fields } ->
    let record_ty = check_look_ty (type_env, typ, pos) in
    (match record_ty with
    | Types.Record (expected_field_types, _) ->
      let field_exprs = List.map (fun field -> field.expr) fields in
      let checked_fields =
        List.map
          (fun field_expr -> transExpr (value_env, type_env, field_expr))
          field_exprs
      in
      let field_names_with_types =
        List.map
          (fun (field, checked_field) -> field.symbol, checked_field.ty, field.pos)
          (List.combine fields checked_fields)
      in
      let _ = type_check_fields field_names_with_types expected_field_types typ pos in
      { translated_expr = { translated_expr = (); pos }; ty = record_ty }
    | found_ty ->
      TigerError.semant_error
        ( "Type "
          ^ Symbol.name typ
          ^ " is "
          ^ Types.show_ty found_ty
          ^ ", but expected a record"
        , pos ))
  | Syntax.SeqExpr exprs -> handle_seq_expr value_env type_env exprs pos
  | Syntax.AssignExpr { var; expr } -> handle_assign_expr value_env type_env var expr pos
  | Syntax.LValueExpr { lvalue } -> handle_lvalue_expr value_env type_env lvalue
  | Syntax.IfExpr { cond; then_arm; else_arm } ->
    handle_if_expr value_env type_env (cond, then_arm, else_arm) pos
  | _ -> TigerError.notImplemented ()

and transBinary = function
  | value_env, type_env, Syntax.Expr { expr = Syntax.BinExpr { left; right; op }; pos } ->
    (* We will use this in the future *)
    (match op with
    | BinaryEq ->
      let { ty = left_ty; _ } = transExpr (value_env, type_env, left)
      and { ty = right_ty; _ } = transExpr (value_env, type_env, right) in
      (match left_ty == right_ty with
      | true ->
        { translated_expr = { translated_expr = (); pos }; ty = Types.Int }
      | false ->
        TigerError.semant_error
          ( Printf.sprintf
              "Type mismatch. Types should be same for equality. Left expression is of \
               type %s and right is %s "
              (Types.show_ty left_ty)
              (Types.show_ty right_ty)
          , pos ))
    | _ ->
      let { ty = ty_left; translated_expr = { pos = pos_left; _ } } =
        transExpr (value_env, type_env, left)
      and { ty = ty_right; translated_expr = { pos = pos_right; _ } } =
        transExpr (value_env, type_env, right)
      in
      expecting_int ty_left pos_left;
      expecting_int ty_right pos_right;
      { translated_expr = { translated_expr = (); pos }; ty = Types.Int })
  | _ -> TigerError.notImplemented ()

and check_look_env (env, name, pos) =
  match Symbol.look (env, name) with
  | Some look -> look
  | None -> TigerError.semant_error ("Variable " ^ Symbol.name name ^ " not found", pos)

and check_look_ty (env, name, pos) =
  match Symbol.look (env, name) with
  | Some look -> look
  | None -> TigerError.semant_error ("Variable " ^ Symbol.name name ^ " not found", pos)

and type_check_args arg_types args pos =
  let arg_types_len = List.length arg_types
  and args_len = List.length args in
  match arg_types_len = args_len with
  | true ->
    let combined = List.combine arg_types args in
    let any_unmatched = List.find_opt (fun (arg_type, arg) -> arg_type != arg) combined in
    (match any_unmatched with
    | Some (arg_type, arg) ->
      TigerError.semant_error
        ( "Types "
          ^ Types.show_ty arg_type
          ^ " and "
          ^ Types.show_ty arg
          ^ " are different"
        , pos )
    | None -> ())
  | false ->
    TigerError.semant_error
      ( "Function takes "
        ^ string_of_int arg_types_len
        ^ " but given "
        ^ string_of_int args_len
      , pos )

(* This function first combines both given and expected names, than zips it with their types if provided.field_id
   This way we get a tuple list of (name, given_record, expected_record) for each such tuple:
   1- both exists and equal -> type check
   2- both exists but are different types -> type mismatch
   3- other -> error depending on which one is missing *)
and type_check_fields given_fields expected_fields record_name pos =
  let given_field_names = List.map (fun (name, _, _) -> name) given_fields
  and expected_field_names = List.map (fun field -> field.field_id) expected_fields in
  let all_names =
    List.sort_uniq
      (fun s1 s2 -> String.compare (Symbol.name s1) (Symbol.name s2))
      (List.concat [ given_field_names; expected_field_names ])
  in
  let names_with_types =
    List.map
      (fun name ->
        ( name
        , List.find_opt (fun (given_name, _, _) -> given_name = name) given_fields
        , List.find_opt (fun field -> field.field_id = name) expected_fields ))
      all_names
  in
  List.map
    (fun (name, given_field_opt, expected_field_opt) ->
      match given_field_opt, expected_field_opt with
      | Some (_, given_ty, _), Some { field_ty = expected_ty; _ } ->
        if given_ty = expected_ty
        then name, given_ty
        else
          TigerError.semant_error
            ( sprintf
                "Type mismatch, expected type %s, but found %s"
                (Types.show_ty given_ty)
                (Types.show_ty expected_ty)
            , pos )
      | Some (given_name, given_ty, pos), None ->
        TigerError.semant_error
          ( sprintf
              "Given gield %s does not exist on record on record %s"
              (Types.show_field { field_id = given_name; field_ty = given_ty })
              (Symbol.name record_name)
          , pos )
      | None, Some expected_ty ->
        TigerError.semant_error
          ( sprintf
              "Expected field %s in record on record %s"
              (Types.show_field expected_ty)
              (Symbol.name record_name)
          , pos )
      | _ -> TigerError.unreachable ())
    names_with_types

and type_check expr = transExpr (Env.baseValueEnv, Env.baseTypeEnv, expr)

and handle_seq_expr value_env type_env exprs pos =
  match exprs with
  | [] -> { translated_expr = { translated_expr = (); pos }; ty = Types.Unit }
  | [ expr ] -> transExpr (value_env, type_env, expr)
  | expr :: exprs ->
    let _ = transExpr (value_env, type_env, expr) in
    handle_seq_expr value_env type_env exprs pos

and handle_assign_expr value_env type_env var expr pos =
  let { translated_expr = _; ty = var_ty } = trans_var value_env type_env var
  and { translated_expr = _; ty = expr_ty } = transExpr (value_env, type_env, expr) in
  match var_ty = expr_ty with
  | true -> { translated_expr = { translated_expr = (); pos }; ty = Types.Unit }
  | false ->
    TigerError.semant_error
      ( Printf.sprintf
          "Type mismatch, assigned variable has type %s, but value is of type %s"
          (Types.show_ty var_ty)
          (Types.show_ty expr_ty)
      , pos )

and handle_lvalue_expr value_env type_env lvalue = trans_var value_env type_env lvalue

and handle_if_expr value_env type_env (cond, then_arm, else_arm_opt) if_pos =
  let { translated_expr = { pos; _ }; ty = cond_ty } =
    transExpr (value_env, type_env, cond)
  in
  let _ = expecting_int cond_ty pos in
  let { ty = then_ty; _ } = transExpr (value_env, type_env, then_arm) in
  match else_arm_opt with
  | Some else_arm ->
    let { translated_expr = { pos; _ }; ty = else_ty } =
      transExpr (value_env, type_env, else_arm)
    in
    if then_ty = else_ty
    then { translated_expr = { translated_expr = (); pos = if_pos }; ty = then_ty }
    else
      TigerError.semant_error
        ( Printf.sprintf
            "If arms does not match. Then arm is type of %s and else is %s"
            (Types.show_ty then_ty)
            (Types.show_ty else_ty)
        , pos )
  | None -> { translated_expr = { translated_expr = (); pos = if_pos }; ty = Types.Unit }

and trans_var value_env type_env var =
  match var with
  | SimpleVar { symbol; pos } ->
    let value_entry = check_look_env (value_env, symbol, pos) in
    (* It must be a var *)
    (match value_entry with
    | VarEntry ty -> { translated_expr = { translated_expr = (); pos }; ty }
    | _ -> TigerError.semant_error ("Can't assign to a function", pos))
  | FieldVar { var; symbol; pos } ->
    let { translated_expr = _; ty = var_ty } = trans_var value_env type_env var in
    (match var_ty with
    | Record (fields, _) ->
      let field_id = symbol in
      (match Types.find_field fields field_id with
      | Some { field_ty; _ } ->
        { translated_expr = { translated_expr = (); pos }; ty = field_ty }
      | None ->
        TigerError.semant_error
          ( Printf.sprintf
              "Field %s does not exist on type %s"
              (Symbol.name field_id)
              (Types.show_ty var_ty)
          , pos ))
    | _ ->
      TigerError.semant_error
        ( Printf.sprintf
            "Variable %s is not a record, field accesses can only be done to records"
            (Syntax.show_var var)
        , pos ))
  | SubscriptVar { var; expr; pos } ->
    let { translated_expr = _; ty = var_ty } = trans_var value_env type_env var in
    (match var_ty with
    | Array (ty, _) ->
      let trans_expr = transExpr (value_env, type_env, expr) in
      let _ = expecting_int trans_expr.ty trans_expr.translated_expr.pos in
      { translated_expr = { translated_expr = (); pos }; ty }
    | _ ->
      TigerError.semant_error
        ( Printf.sprintf
            "Variable %s is not an array, subscript accesses can only be done to arrays"
            (Syntax.show_var var)
        , pos ))
;;