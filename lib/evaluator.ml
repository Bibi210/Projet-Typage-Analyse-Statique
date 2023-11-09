open Ast
open Baselib
open Helpers
module Env = Map.Make (String)

type eval_errors =
  | TyperCheck of expr
  | Unbound of variable
  | LambdaReduction of expr (* When the body of the function is only Fix of itself *)
  | Timeout of expr
  | OutOfBound of expr

exception InternalError of eval_errors

exception
  EvalError of
    { message : string
    ; location : Helpers.position
    }

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
         Assign { area = alphaReverser area; nval = alphaReverser nval })
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
;;

let memory = ref Env.empty

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
    | Ref { epre = Var _; _ } -> expr
    | Ref x ->
      let addr = Helpers.symbolGenerator "ptr" in
      memory := Env.add addr x !memory;
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
       | Ref { epre = Var addr; _ } ->
         (match Env.find_opt addr !memory with
          | Some _ ->
            memory := Env.add addr nval !memory;
            writeConvertion (Const Unit)
          | None -> raise (InternalError (OutOfBound e)))
       | _ -> raise (InternalError (TyperCheck e)))
    | Lambda _ | Const _ | Var _ -> expr
  in
  memory := Env.empty;
  let e = alphaConverter e in
  try alphaReverser (betaReduce' e) with
  | InternalError (TyperCheck e) ->
    raise (EvalError { message = "Type Checking Leak"; location = e.epos })
  | InternalError (Unbound v) ->
    raise (EvalError { message = "Unbound variable"; location = v.vpos })
  | InternalError (LambdaReduction e) ->
    raise (EvalError { message = "Lambda Reduction Failure"; location = e.epos })
  | InternalError (Timeout e) ->
    let msg = Prettyprinter.string_of_expr e in
    raise (EvalError { message = "Timeout on : " ^ msg; location = e.epos })
;;
