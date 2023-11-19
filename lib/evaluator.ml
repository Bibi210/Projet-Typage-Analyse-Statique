open Ast
open Baselib
open Helpers

type eval_errors =
  | TyperCheck of expr
  | Unbound of variable
  | LambdaReduction of expr (* When the body of the function is only Fix of itself *)
  | Timeout of expr
  | OutOfBound of expr
  | NonExhaustivePattern of expr

exception InternalError of eval_errors

exception
  EvalError of
    { message : string
    ; location : Helpers.position
    }

let rec alphaConvertPattern pattern env =
  let newenv, newpre =
    match pattern.pnode with
    | LitteralPattern a -> env, LitteralPattern a
    | VarPattern a ->
      let newid = symbolGenerator a in
      Env.add a newid env, VarPattern newid
    | TuplePattern a ->
      let newenv, newpre =
        List.fold_left
          (fun (env, pre) x ->
            let newenv, newpre = alphaConvertPattern x env in
            newenv, newpre :: pre)
          (env, [])
          a
      in
      newenv, TuplePattern (List.rev newpre)
    | ConstructorPattern { constructor_ident; content } ->
      let newenv, newpre = alphaConvertPattern content env in
      newenv, ConstructorPattern { constructor_ident; content = newpre }
  in
  newenv, { pnode = newpre; ppos = pattern.ppos; typAnnotation = pattern.typAnnotation }
;;

let rec alphaRevertPattern pattern =
  let writeConvertion pnode = { pattern with pnode } in
  match pattern.pnode with
  | LitteralPattern _ -> pattern
  | VarPattern x -> writeConvertion (VarPattern (getNameFromSymbol x))
  | TuplePattern x -> writeConvertion (TuplePattern (List.map alphaRevertPattern x))
  | ConstructorPattern { constructor_ident; content } ->
    writeConvertion
      (ConstructorPattern { constructor_ident; content = alphaRevertPattern content })
;;

let rec alphaReverser expr =
  let renameVarId oldVar = { oldVar with id = getNameFromSymbol oldVar.id } in
  { expr with
    epre =
      (match expr.epre with
       | Var x -> Var (getNameFromSymbol x)
       | Lambda { varg; body } ->
         Lambda { varg = renameVarId varg; body = alphaReverser body }
       | App { func; carg } ->
         App { func = alphaReverser func; carg = alphaReverser carg }
       | Const _ -> expr.epre
       | If { cond; tbranch; fbranch } ->
         If
           { cond = alphaReverser cond
           ; tbranch = alphaReverser tbranch
           ; fbranch = alphaReverser fbranch
           }
       | Let { varg; init; body } ->
         Let
           { varg = renameVarId varg
           ; init = alphaReverser init
           ; body = alphaReverser body
           }
       | Fix { varg; body } -> Fix { varg = renameVarId varg; body = alphaReverser body }
       | BinOp x ->
         BinOp { x with larg = alphaReverser x.larg; rarg = alphaReverser x.rarg }
       | Ref x -> Ref (alphaReverser x)
       | Deref x -> Deref (alphaReverser x)
       | Seq { left; right } ->
         Seq { left = alphaReverser left; right = alphaReverser right }
       | UnOp x -> UnOp { x with arg = alphaReverser x.arg }
       | Assign { area; nval } ->
         Assign { area = alphaReverser area; nval = alphaReverser nval }
       | Construct { constructor; args } ->
         Construct { constructor; args = alphaReverser args }
       | Tuple x -> Tuple (List.map alphaReverser x)
       | Match { matched; cases } ->
         let cases =
           List.map
             (fun { pattern; consequence } ->
               let newpattern = alphaRevertPattern pattern in
               { pattern = newpattern; consequence = alphaReverser consequence })
             cases
         in
         Match { matched = alphaReverser matched; cases })
  }
;;

let alphaConverter expr =
  let changeVarId oldVar newid = { oldVar with id = newid } in
  let rec alphaConverter' expr env =
    let writeConvertion epre = { expr with epre } in
    match expr.epre with
    | Var x ->
      (match Env.find_opt x env with
       | Some new_id -> writeConvertion (Var new_id)
       | None -> expr)
    | Lambda { varg; body } ->
      let new_id = symbolGenerator varg.id in
      let new_env = Env.add varg.id new_id env in
      writeConvertion
        (Lambda { varg = changeVarId varg new_id; body = alphaConverter' body new_env })
    | App { func; carg } ->
      writeConvertion
        (App { func = alphaConverter' func env; carg = alphaConverter' carg env })
    | Const _ -> expr
    | If { cond; tbranch; fbranch } ->
      writeConvertion
        (If
           { cond = alphaConverter' cond env
           ; tbranch = alphaConverter' tbranch env
           ; fbranch = alphaConverter' fbranch env
           })
    | Let { varg; init; body } ->
      let new_id = symbolGenerator varg.id in
      let new_env = Env.add varg.id new_id env in
      writeConvertion
        (Let
           { varg = changeVarId varg new_id
           ; init = alphaConverter' init env
           ; body = alphaConverter' body new_env
           })
    | Fix { varg; body } ->
      let new_id = symbolGenerator varg.id in
      let new_env = Env.add varg.id new_id env in
      writeConvertion
        (Fix { varg = changeVarId varg new_id; body = alphaConverter' body new_env })
    | BinOp x ->
      writeConvertion
        (BinOp
           { x with larg = alphaConverter' x.larg env; rarg = alphaConverter' x.rarg env })
    | UnOp x -> writeConvertion (UnOp { x with arg = alphaConverter' x.arg env })
    | Ref x -> writeConvertion (Ref (alphaConverter' x env))
    | Deref x -> writeConvertion (Deref (alphaConverter' x env))
    | Seq { left; right } ->
      writeConvertion
        (Seq { left = alphaConverter' left env; right = alphaConverter' right env })
    | Assign { area; nval } ->
      writeConvertion
        (Assign { area = alphaConverter' area env; nval = alphaConverter' nval env })
    | Construct { constructor; args } ->
      writeConvertion (Construct { constructor; args = alphaConverter' args env })
    | Tuple x -> writeConvertion (Tuple (List.map (fun x -> alphaConverter' x env) x))
    | Match { matched; cases } ->
      let cases =
        List.map
          (fun { pattern; consequence } ->
            let newenv, newpattern = alphaConvertPattern pattern env in
            { pattern = newpattern; consequence = alphaConverter' consequence newenv })
          cases
      in
      writeConvertion (Match { matched = alphaConverter' matched env; cases })
  in
  alphaConverter' expr Env.empty
;;

let rec substitute id other expr =
  let fix = substitute id other in
  let writeConvertion epre = { expr with epre } in
  match expr.epre with
  | Var x -> if x = id then other else expr
  | Lambda { varg; body } -> writeConvertion (Lambda { varg; body = fix body })
  | App { func; carg } -> writeConvertion (App { func = fix func; carg = fix carg })
  | Const _ -> expr
  | If { cond; tbranch; fbranch } ->
    writeConvertion (If { cond = fix cond; tbranch = fix tbranch; fbranch = fix fbranch })
  | Let { varg; init; body } ->
    writeConvertion (Let { varg; init = fix init; body = fix body })
  | Fix { varg; body } -> writeConvertion (Fix { varg; body = fix body })
  | BinOp x -> writeConvertion (BinOp { x with larg = fix x.larg; rarg = fix x.rarg })
  | UnOp x -> writeConvertion (UnOp { x with arg = fix x.arg })
  | Ref x -> writeConvertion (Ref (fix x))
  | Deref x -> writeConvertion (Deref (fix x))
  | Seq { left; right } -> writeConvertion (Seq { left = fix left; right = fix right })
  | Assign { area; nval } -> writeConvertion (Assign { area = fix area; nval = fix nval })
  | Construct { constructor; args } ->
    writeConvertion (Construct { constructor; args = fix args })
  | Tuple x -> writeConvertion (Tuple (List.map fix x))
  | Match { matched; cases } ->
    let cases =
      List.map
        (fun { pattern; consequence } -> { pattern; consequence = fix consequence })
        cases
    in
    writeConvertion (Match { matched = fix matched; cases })
;;

let substituteEnv expr env =
  let assoc = Env.bindings env in
  List.fold_left (fun expr (id, other) -> substitute id other expr) expr assoc
;;

let memory = ref Env.empty

let rec exprMatchPattern expr patt env =
  match expr.epre, patt.pnode with
  | _, VarPattern b -> Some (Env.add b expr env)
  | Const a, LitteralPattern b when a = b -> Some env
  | Tuple x, TuplePattern y when List.length x = List.length y ->
    (try
       Some
         (List.fold_left2
            (fun env x y ->
              match exprMatchPattern x y env with
              | Some env -> env
              | None -> raise Exit)
            env
            x
            y)
     with
     | Exit -> None)
  | Construct { constructor; args }, ConstructorPattern { constructor_ident; content } ->
    if constructor.id = constructor_ident then exprMatchPattern args content env else None
  | _, _ -> None
;;

let betaReduce e =
  let rec betaReduce' expr =
    let writeConvertion epre = { expr with epre } in
    match expr.epre with
    | App { func = { epre = Lambda { varg; body }; _ }; carg } ->
      let carg = betaReduce' carg in
      betaReduce' (substitute varg.id carg body)
    | App { func; carg } ->
      (match betaReduce' func with
       | x when x = func -> writeConvertion (App { func; carg }) (* Redex but not func *)
       | func -> betaReduce' (writeConvertion (App { func; carg })))
    | If { cond; tbranch; fbranch } ->
      (match cond.epre with
       | Const (Int 0) -> betaReduce' tbranch
       | Const (Int _) -> betaReduce' fbranch
       | _ ->
         (match betaReduce' cond with
          | x when x = cond -> raise (InternalError (TyperCheck cond))
          | cond -> betaReduce' (writeConvertion (If { cond; tbranch; fbranch }))))
    | Let { varg; init; body } ->
      let init = betaReduce' init in
      betaReduce' (substitute varg.id init body)
    | Fix { varg; body } -> betaReduce' (substitute varg.id expr (alphaConverter body))
    | BinOp { op; larg; rarg } ->
      let op = find_binop op in
      let larg = betaReduce' larg in
      let rarg = betaReduce' rarg in
      writeConvertion (op.func [ larg.epre; rarg.epre ])
    | UnOp { op; arg } ->
      let op = find_unop op in
      let arg = betaReduce' arg in
      writeConvertion (op.func [ arg.epre ])
    | Ref { epre = Var x; _ } when getNameFromSymbol x = "ptr" -> expr
    | Ref x ->
      let addr = Helpers.symbolGenerator "ptr" in
      memory := Env.add addr (betaReduce' x) !memory;
      writeConvertion (Ref { epre = Var addr; epos = x.epos; etyp_annotation = None })
    | Deref x ->
      (match x.epre with
       | Ref { epre = Var addr; _ } ->
         (match Env.find_opt addr !memory with
          | Some x -> x
          | None -> raise (InternalError (OutOfBound e)))
       | _ -> raise (InternalError (TyperCheck e)))
    | Seq { left; right } ->
      let _ = betaReduce' left in
      let right = betaReduce' right in
      right
    | Assign { area; nval } ->
      let nval = betaReduce' nval in
      let addr = betaReduce' area in
      (match addr.epre with
       | Ref { epre = Var addr; _ } when getNameFromSymbol addr = "ptr"  ->
         (match Env.find_opt addr !memory with
          | Some _ ->
            memory := Env.add addr nval !memory;
            writeConvertion (Const Unit)
          | None -> raise (InternalError (OutOfBound e)))
       | _ -> raise (InternalError (TyperCheck e)))
    | Construct { constructor; args } ->
      let args = betaReduce' args in
      writeConvertion (Construct { constructor; args })
    | Tuple x -> writeConvertion (Tuple (List.map betaReduce' x))
    | Match { matched; cases } ->
      let reduced = betaReduce' matched in
      (match
         List.fold_left
           (fun acc { pattern; consequence } ->
             match acc with
             | Some _ -> acc
             | None ->
               (match exprMatchPattern reduced pattern Env.empty with
                | Some env -> Some (substituteEnv consequence env)
                | None -> None))
           None
           cases
       with
       | Some x -> betaReduce' x
       | None -> raise (InternalError (NonExhaustivePattern e)))
    | Lambda _ | Const _ | Var _ -> expr
  in
  memory := Env.empty;
  let e = alphaConverter e in
  try alphaReverser (betaReduce' e) with
  | InternalError err ->
    raise
      (match err with
       | TyperCheck e -> EvalError { message = "Type Checking Leak"; location = e.epos }
       | Unbound v -> EvalError { message = "Unbound variable"; location = v.vpos }
       | LambdaReduction e ->
         EvalError { message = "Lambda Reduction Failure"; location = e.epos }
       | Timeout e ->
         let msg = Prettyprinter.string_of_expr e in
         EvalError { message = "Timeout on : " ^ msg; location = e.epos }
       | OutOfBound e -> EvalError { message = "Out of bound"; location = e.epos }
       | NonExhaustivePattern e ->
         let msg = Prettyprinter.string_of_expr e in
         EvalError
           { message = "Non exhaustive pattern match on : " ^ msg; location = e.epos })
;;
