open TigerError
open Translated
open Syntax
open Types
open Printf
open Env
open Utils

let expecting_ty expected_ty given_ty pos =
  match expected_ty == given_ty with
  | true ->
      ()
  | false ->
      TigerError.semant_error
        ( sprintf
            "Type mismatch: Expected type %s, found %s"
            (Types.show_ty expected_ty)
            (Types.show_ty given_ty)
        , pos )


let expecting_int = expecting_ty Types.Int

(* Returns whether an assignment to a value type checks *)
let assignment_type_checks lvalue_ty value_ty =
  match (lvalue_ty, value_ty) with
  | lvalue_ty, value_ty when lvalue_ty == value_ty ->
      true
  | Record _, Nil ->
      true
  | _ ->
      false


let check_look_env (env, name, pos) =
  match Symbol.look (env, name) with
  | Some look ->
      look
  | None ->
      TigerError.semant_error
        (sprintf "Variable %s not found" (Symbol.name name), pos)


let check_look_ty (env, name, pos) =
  match Symbol.look (env, name) with
  | Some look ->
      look
  | None ->
      TigerError.semant_error
        (sprintf "Variable %s not found" (Symbol.name name), pos)


let type_check_args arg_types args pos =
  let arg_types_len = List.length arg_types
  and args_len = List.length args in
  match arg_types_len = args_len with
  | true ->
      let combined = List.combine arg_types args in
      let any_unmatched =
        List.find_opt (fun (arg_type, arg) -> arg_type != arg) combined
      in
      ( match any_unmatched with
      | Some (arg_type, arg) ->
          TigerError.semant_error
            ( sprintf
                "Type mismatch: function takes %s but an expression of type %s \
                 is given"
                (Types.show_ty arg_type)
                (Types.show_ty arg)
            , pos )
      | None ->
          () )
  | false ->
      TigerError.semant_error
        ( sprintf
            "Function takes %s arguments, but given %s"
            (string_of_int arg_types_len)
            (string_of_int args_len)
        , pos )


(* This function first combines both given and expected names, than zips it with their types if provided.field_id
   This way we get a tuple list of (name, given_record, expected_record) for each such tuple:
   1- both exists and equal -> type check
   2- both exists but are different types -> type mismatch
   3- other -> error depending on which one is missing *)
let type_check_fields given_fields expected_fields record_name pos =
  let given_field_names = List.map (fun (name, _, _) -> name) given_fields
  and expected_field_names =
    List.map (fun field -> field.field_id) expected_fields
  in
  let all_names =
    List.sort_uniq
      (fun s1 s2 -> String.compare (Symbol.name s1) (Symbol.name s2))
      (List.concat [ given_field_names; expected_field_names ])
  in
  let names_with_types =
    List.map
      (fun name ->
        ( name
        , List.find_opt
            (fun (given_name, _, _) -> given_name = name)
            given_fields
        , List.find_opt (fun field -> field.field_id = name) expected_fields )
        )
      all_names
  in
  List.map
    (fun (name, given_field_opt, expected_field_opt) ->
      match (given_field_opt, expected_field_opt) with
      | Some (_, given_ty, _), Some { field_ty = expected_ty; _ } ->
          if given_ty = expected_ty
          then (name, given_ty)
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
                (Types.show_field
                   { field_id = given_name; field_ty = given_ty } )
                (Symbol.name record_name)
            , pos )
      | None, Some expected_ty ->
          TigerError.semant_error
            ( sprintf
                "Expected field %s in record on record %s"
                (Types.show_field expected_ty)
                (Symbol.name record_name)
            , pos )
      | _ ->
          TigerError.unreachable () )
    names_with_types


let rec trans_expr (value_env, type_env, Syntax.Expr { expr; pos }) =
  let rec handle_seq_expr value_env type_env exprs pos =
    match exprs with
    | [] ->
        { translated_expr = { translated_expr = (); pos }; ty = Types.Unit }
    | [ expr ] ->
        trans_expr (value_env, type_env, expr)
    | expr :: exprs ->
        let _ = trans_expr (value_env, type_env, expr) in
        handle_seq_expr value_env type_env exprs pos
  and handle_assign_expr value_env type_env var expr pos =
    let { translated_expr = _; ty = var_ty } = trans_var value_env type_env var
    and { translated_expr = _; ty = expr_ty } =
      trans_expr (value_env, type_env, expr)
    in
    match assignment_type_checks var_ty expr_ty with
    | true ->
        { translated_expr = { translated_expr = (); pos }; ty = Types.Unit }
    | false ->
        TigerError.semant_error
          ( sprintf
              "Type mismatch, assigned variable has type %s, but value is of \
               type %s"
              (Types.show_ty var_ty)
              (Types.show_ty expr_ty)
          , pos )
  and handle_lvalue_expr value_env type_env lvalue =
    trans_var value_env type_env lvalue
  and handle_if_expr value_env type_env (cond, then_arm, else_arm_opt) if_pos =
    let { translated_expr = { pos; _ }; ty = cond_ty } =
      trans_expr (value_env, type_env, cond)
    in
    let _ = expecting_int cond_ty pos in
    let { ty = then_ty; _ } = trans_expr (value_env, type_env, then_arm) in
    match else_arm_opt with
    | Some else_arm ->
        let { translated_expr = { pos; _ }; ty = else_ty } =
          trans_expr (value_env, type_env, else_arm)
        in
        if then_ty = else_ty
        then
          { translated_expr = { translated_expr = (); pos = if_pos }
          ; ty = then_ty
          }
        else
          TigerError.semant_error
            ( sprintf
                "If arms does not match. Then arm is type of %s and else is %s"
                (Types.show_ty then_ty)
                (Types.show_ty else_ty)
            , pos )
    | None ->
        (* TODO: More descriptive error message *)
        let _ = expecting_ty Types.Unit then_ty pos in
        { translated_expr = { translated_expr = (); pos = if_pos }
        ; ty = Types.Unit
        }
  and handle_while_expr value_env type_env (cond, body) pos =
    let { translated_expr = { pos = cond_pos; _ }; ty = cond_ty } =
      trans_expr (value_env, type_env, cond)
    in
    let _ = expecting_int cond_ty cond_pos in
    let { ty = body_ty; translated_expr = { pos = body_pos; _ } } =
      trans_expr (value_env, type_env, body)
    in
    match body_ty with
    | Types.Unit ->
        { translated_expr = { translated_expr = (); pos }; ty = Types.Unit }
    | body_ty ->
        TigerError.semant_error
          ( sprintf
              "Body of a while must produce no value, which means it must \
               return unit. But this body has type %s"
              (Types.show_ty body_ty)
          , body_pos )
  and handle_for_expr value_env type_env (var, from, to', body) pos =
    let { ty = from_ty; translated_expr = { pos = from_pos; _ } } =
      trans_expr (value_env, type_env, from)
    and { ty = to_ty; translated_expr = { pos = to_pos; _ } } =
      trans_expr (value_env, type_env, to')
    in
    let _ = expecting_int from_ty from_pos
    and _ = expecting_int to_ty to_pos in
    let value_env = Symbol.enter (value_env, var, VarEntry Types.Int) in
    let { ty = body_ty; translated_expr = { pos = body_pos; _ } } =
      trans_expr (value_env, type_env, body)
    in
    match body_ty with
    | Types.Unit ->
        { translated_expr = { translated_expr = (); pos }; ty = Types.Unit }
    | body_ty ->
        TigerError.semant_error
          ( sprintf
              "Body of a for must produce no value, which means it must return \
               unit. But this body has type %s"
              (Types.show_ty body_ty)
          , body_pos )
  and handle_let_expr value_env type_env (decls, body) _pos =
    let value_env, type_env = trans_decls value_env type_env decls in
    trans_expr (value_env, type_env, body)
  and handle_array_expr
      value_env type_env (typ_symbol, size_expr, init_expr) pos =
    let ty = check_look_ty (type_env, typ_symbol, pos) in
    match ty with
    | Array (value_ty, _) ->
        let { ty = size_ty; translated_expr = { pos = size_pos; _ } } =
          trans_expr (value_env, type_env, size_expr)
        and { ty = init_ty; translated_expr = { pos = init_pos; _ } } =
          trans_expr (value_env, type_env, init_expr)
        in
        let _ = expecting_int size_ty size_pos
        and _ = expecting_ty value_ty init_ty init_pos in
        { ty; translated_expr = { pos; translated_expr = () } }
    | ty ->
        TigerError.semant_error
          (sprintf "Expected array type, found %s" (Types.show_ty ty), pos)
  and handle_call_expr value_env type_env (func, args) pos =
    let entry = check_look_env (value_env, func, pos) in
    match entry with
    | FunEntry { argTypes; return_type } ->
        let args_checked =
          List.map (fun arg -> trans_expr (value_env, type_env, arg)) args
        in
        type_check_args argTypes (List.map (fun arg -> arg.ty) args_checked) pos ;
        { translated_expr = { translated_expr = (); pos }; ty = return_type }
    | other ->
        TigerError.semant_error
          ( sprintf
              "Variable %s is expected to be function, but found %s "
              (Symbol.name func)
              (show_envEntry other)
          , pos )
  and handle_binary_expr value_env type_env (left, right, op) pos =
    match op with
    | BinaryEq | BinaryLtgt ->
        let { ty = left_ty; _ } = trans_expr (value_env, type_env, left)
        and { ty = right_ty; _ } = trans_expr (value_env, type_env, right) in
        ( match left_ty == right_ty with
        | true ->
            { translated_expr = { translated_expr = (); pos }; ty = Types.Int }
        | false ->
            TigerError.semant_error
              ( sprintf
                  "Type mismatch. Types should be same for equality. Left \
                   expression is of type %s and right is %s "
                  (Types.show_ty left_ty)
                  (Types.show_ty right_ty)
              , pos ) )
    | _ ->
        let { ty = ty_left; translated_expr = { pos = pos_left; _ } } =
          trans_expr (value_env, type_env, left)
        and { ty = ty_right; translated_expr = { pos = pos_right; _ } } =
          trans_expr (value_env, type_env, right)
        in
        expecting_int ty_left pos_left ;
        expecting_int ty_right pos_right ;
        { translated_expr = { translated_expr = (); pos }; ty = Types.Int }
  and handle_record_expr value_env type_env (typ, fields) pos =
    let record_ty = check_look_ty (type_env, typ, pos) in
    match record_ty with
    | Types.Record (expected_field_types, _) ->
        let field_exprs = List.map (fun field -> field.expr) fields in
        let checked_fields =
          List.map
            (fun field_expr -> trans_expr (value_env, type_env, field_expr))
            field_exprs
        in
        let field_names_with_types =
          List.map
            (fun (field, checked_field) ->
              (field.symbol, checked_field.ty, field.pos) )
            (List.combine fields checked_fields)
        in
        let _ =
          type_check_fields field_names_with_types expected_field_types typ pos
        in
        { translated_expr = { translated_expr = (); pos }; ty = record_ty }
    | found_ty ->
        TigerError.semant_error
          ( sprintf
              "Type mismatch: Expected record but type %s is %s"
              (Symbol.name typ)
              (Types.show_ty found_ty)
          , pos )
  in
  match expr with
  | Syntax.IntExpr _ ->
      { translated_expr = { translated_expr = (); pos }; ty = Types.Int }
  | Syntax.NilExpr ->
      { translated_expr = { translated_expr = (); pos }; ty = Types.Nil }
  | Syntax.StringExpr _ ->
      { translated_expr = { translated_expr = (); pos }; ty = Types.String }
  | Syntax.CallExpr { func; args } ->
      handle_call_expr value_env type_env (func, args) pos
  | Syntax.BinExpr { left; right; op } ->
      handle_binary_expr value_env type_env (left, right, op) pos
  | Syntax.RecordExpr { typ; fields } ->
      handle_record_expr value_env type_env (typ, fields) pos
  | Syntax.SeqExpr exprs ->
      handle_seq_expr value_env type_env exprs pos
  | Syntax.AssignExpr { var; expr } ->
      handle_assign_expr value_env type_env var expr pos
  | Syntax.LValueExpr { lvalue } ->
      handle_lvalue_expr value_env type_env lvalue
  | Syntax.IfExpr { cond; then_arm; else_arm } ->
      handle_if_expr value_env type_env (cond, then_arm, else_arm) pos
  | Syntax.WhileExpr { cond; body } ->
      handle_while_expr value_env type_env (cond, body) pos
  | Syntax.ForExpr { var; from; to'; body; _ } ->
      handle_for_expr value_env type_env (var, from, to', body) pos
  | Syntax.BreakExpr ->
      { translated_expr = { translated_expr = (); pos }; ty = Types.Unit }
  | Syntax.LetExpr { decls; body } ->
      handle_let_expr value_env type_env (decls, body) pos
  | Syntax.ArrayExpr { typ; size; init_value } ->
      handle_array_expr value_env type_env (typ, size, init_value) pos


and trans_var value_env type_env var =
  match var with
  | SimpleVar { symbol; pos } ->
      let value_entry = check_look_env (value_env, symbol, pos) in
      (* It must be a var *)
      (
        ( match value_entry with
        | VarEntry ty ->
            { translated_expr = { translated_expr = (); pos }; ty }
        | FunEntry _ ->
            TigerError.semant_error ("Functions can't be an lvalue", pos) ))
  | FieldVar { var; symbol; pos } ->
      let { translated_expr = _; ty = var_ty } =
        trans_var value_env type_env var
      in
      ( match var_ty with
      | Record (fields, _) ->
          let field_id = symbol in
          ( match Types.find_field fields field_id with
          | Some { field_ty; _ } ->
              { translated_expr = { translated_expr = (); pos }; ty = field_ty }
          | None ->
              TigerError.semant_error
                ( sprintf
                    "Field %s does not exist on type %s"
                    (Symbol.name field_id)
                    (Types.show_ty var_ty)
                , pos ) )
      | _ ->
          TigerError.semant_error
            ( sprintf
                "Variable %s is not a record, field accesses can only be done \
                 to records"
                (Syntax.show_var var)
            , pos ) )
  | SubscriptVar { var; expr; pos } ->
      let { translated_expr = _; ty = var_ty } =
        trans_var value_env type_env var
      in
      ( match var_ty with
      | Array (ty, _) ->
          let trans_expr = trans_expr (value_env, type_env, expr) in
          let _ = expecting_int trans_expr.ty trans_expr.translated_expr.pos in
          { translated_expr = { translated_expr = (); pos }; ty }
      | _ ->
          TigerError.semant_error
            ( sprintf
                "Variable %s is not an array, subscript accesses can only be \
                 done to arrays"
                (Syntax.show_var var)
            , pos ) )


and trans_decls value_env type_env = function
  | decl :: decls ->
      let value_env, type_env = trans_decl value_env type_env decl in
      trans_decls value_env type_env decls
  | [] ->
      (value_env, type_env)


and trans_decl value_env type_env = function
  | VarDecl { name; typ; value; _ } ->
      let { ty = value_ty; _ } = trans_expr (value_env, type_env, value) in
      ( match typ with
      | None ->
          (* Infer the type from right hand side *)
          let value_env = Symbol.enter (value_env, name, VarEntry value_ty) in
          (value_env, type_env)
      | Some (Type { symbol; pos }) ->
          let decl_ty = check_look_ty (type_env, symbol, pos) in
          ( match assignment_type_checks decl_ty value_ty with
          | true ->
              (* Assigned value can be nil, therefore we use the declared type *)
              let value_env =
                Symbol.enter (value_env, name, VarEntry decl_ty)
              in
              (value_env, type_env)
          | false ->
              TigerError.semant_error
                ( sprintf
                    "Declared type is %s but right hand side is the type %s"
                    (Types.show_ty decl_ty)
                    (Types.show_ty value_ty)
                , pos ) ) )
  | TypeDecls [ TypeDecl { name; decl; _ } ] ->
      let ty = trans_ty type_env decl in
      let type_env = Symbol.enter (type_env, name, ty) in
      (value_env, type_env)
  | FunctionDecls [ FunDecl { name; params; return_type; body; _ } ] ->
      let param_names_with_tys =
        List.map
          (function
            | TypedField { name; typ; pos; _ } ->
                (name, check_look_ty (type_env, typ, pos)) )
          params
      in
      let return_ty_opt =
        Option.map
          (function
            | Type { symbol; pos } -> check_look_ty (type_env, symbol, pos) )
          return_type
      in
      let return_ty =
        match return_ty_opt with
        | Some return_ty ->
            return_ty
        | None ->
            Types.Unit
      in
      let param_tys = List.map (fun (_, ty) -> ty) param_names_with_tys in
      (* Function declaration itself will live outside the function as well, but parameters do not *)
      let value_env =
        Symbol.enter
          ( value_env
          , name
          , FunEntry { argTypes = param_tys; return_type = return_ty } )
      in
      let value_env' =
        Symbol.enter_all
          ( value_env
          , List.map
              (fun (name, ty) -> (name, VarEntry ty))
              param_names_with_tys )
      in
      let { ty = body_ty
          ; translated_expr = { pos = body_pos; translated_expr = () }
          } =
        trans_expr (value_env', type_env, body)
      in
      let _ = expecting_ty return_ty body_ty body_pos in
      (value_env, type_env)
  | _ -> __ ()


and trans_ty type_env = function
  | RecordDecl { fields; _ } ->
      let fields =
        List.map
          (function
            | TypedField { name; typ; pos; _ } ->
                { field_id = name
                ; field_ty = check_look_ty (type_env, typ, pos)
                } )
          fields
      in
      Record (fields, ref ())
  | NameDecl { name; pos } ->
      check_look_ty (type_env, name, pos)
  | ArrayDecl { name; pos } ->
      Array (check_look_ty (type_env, name, pos), ref ())


let type_check expr = trans_expr (baseValueEnv, baseTypeEnv, expr)
