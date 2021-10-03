type depth = int

type esc_env = (depth * bool ref) Symbol.table

let check_escape (env, curr_depth, symbol) =
  match Symbol.look (env, symbol) with
  | Some (depth, escape_ref) ->
      Printf.printf
        "Found: %s, escape: %B\n"
        (Symbol.show_symbol symbol)
        (curr_depth > depth) ;
      if curr_depth > depth then escape_ref := true
  | None ->
      Printf.printf "Not found: %s\n" (Symbol.show_symbol symbol) ;
      ()


let rec traverse_var ((env, curr_depth, var) : esc_env * depth * Syntax.var) =
  let symbol_opt = Syntax.extract_symbol var
  and parent_opt = Syntax.extract_parent_var var in
  ( match parent_opt with
  | Some parent_var ->
      traverse_var (env, curr_depth, parent_var)
  | None ->
      () ) ;
  match symbol_opt with
  | Some symbol ->
      check_escape (env, curr_depth, symbol)
  | None ->
      ()


and traverse_expr
    ((env, curr_depth, Syntax.Expr { expr; _ }) : esc_env * depth * Syntax.expr)
    =
  match expr with
  | Syntax.NilExpr | Syntax.IntExpr _ | Syntax.StringExpr _ | Syntax.BreakExpr
    ->
      ()
  | Syntax.CallExpr { func; args } ->
      check_escape (env, curr_depth, func) ;
      ignore (List.map (fun expr -> traverse_expr (env, curr_depth, expr)) args)
  | Syntax.BinExpr { left; right; _ } ->
      traverse_expr (env, curr_depth, left) ;
      traverse_expr (env, curr_depth, right) ;
      ()
  | Syntax.RecordExpr { fields; _ } ->
      ignore
        ((List.map (fun ({ expr; _ } : Syntax.field) ->
              traverse_expr (env, curr_depth, expr) ) )
           fields )
  | Syntax.SeqExpr exprs ->
      ignore
        (List.map (fun expr -> traverse_expr (env, curr_depth, expr)) exprs)
  | Syntax.AssignExpr { var; expr } ->
      traverse_var (env, curr_depth, var) ;
      traverse_expr (env, curr_depth, expr)
  | Syntax.LValueExpr { lvalue } ->
      traverse_var (env, curr_depth, lvalue) ;
      ()
  | Syntax.IfExpr { cond; then_arm; else_arm } ->
      traverse_expr (env, curr_depth, cond) ;
      traverse_expr (env, curr_depth, then_arm) ;
      ignore
        (Option.map
           (fun else_arm -> traverse_expr (env, curr_depth, else_arm))
           else_arm )
  | Syntax.WhileExpr { cond; body } ->
      traverse_expr (env, curr_depth, cond) ;
      traverse_expr (env, curr_depth, body) ;
      ()
  | Syntax.ForExpr { var; escape; from; to'; body } ->
      traverse_expr (env, curr_depth, from) ;
      traverse_expr (env, curr_depth, to') ;
      (* For declares new variable *)
      let env = Symbol.enter (env, var, (curr_depth, escape)) in
      traverse_expr (env, curr_depth, body) ;
      ()
  | Syntax.LetExpr { decls; body } ->
      let env = traverse_decls (env, curr_depth, decls) in
      traverse_expr (env, curr_depth, body) ;
      ()
  | Syntax.ArrayExpr { size; init_value; _ } ->
      traverse_expr (env, curr_depth, size) ;
      traverse_expr (env, curr_depth, init_value) ;
      ()


and traverse_decls
    ((env, curr_depth, decls) : esc_env * depth * Syntax.decl list) =
  List.fold_left
    (fun env decl -> traverse_decl (env, curr_depth, decl))
    env
    decls


and traverse_decl ((env, curr_depth, decl) : esc_env * depth * Syntax.decl) =
  match decl with
  | Syntax.VarDecl { name; escape; _ } ->
      Symbol.enter (env, name, (curr_depth, escape))
  | Syntax.FunctionDecls fundecls ->
      let traverse_fun_decl (env, fun_decl) =
        match fun_decl with
        | Syntax.FunDecl { body; params; _ } ->
            let enter_param env (Syntax.TypedField { name; escape; _ }) =
              Symbol.enter (env, name, (curr_depth + 1, escape))
            in
            let env = List.fold_left enter_param env params in
            traverse_expr (env, curr_depth + 1, body) ;
            env
      in
      List.fold_left
        (fun env fun_decl -> traverse_fun_decl (env, fun_decl))
        env
        fundecls
  | Syntax.TypeDecls _ ->
      env


let find_escape expr = traverse_expr (Symbol.empty, 0, expr)
