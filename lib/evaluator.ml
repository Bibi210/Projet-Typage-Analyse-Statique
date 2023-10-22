open Ast
open Baselib
open Helpers
module Env = Map.Make (String)

type eval_errors =
  | TyperCheck of expr
  | Unbound of variable
  | LambdaReduction of expr (* When the body of the function is only Fix of itself *)
  | Timeout of expr

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
       | UnOp x -> UnOp { x with arg = alphaReverser x.arg })
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
    | Lambda _ | Const _ | Var _ -> expr
  in
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
