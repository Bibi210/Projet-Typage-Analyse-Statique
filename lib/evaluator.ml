open Ast
open Helpers
module Env = Map.Make (String)

type eval_errors =
  | TyperCheckFAIL of expr
  | Unbound of variable

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
           })
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
;;

let betaReduce e =
  let rec betaReduce' expr =
    let writeConvertion epre = { expr with epre } in
    match expr.epre with
    | App { func; carg } ->
      (match func.epre with
       | Lambda { varg; body } -> betaReduce' (substitute varg.id carg body)
       | _ ->
         let func =
           match betaReduce' func with
           | x when x = func -> raise (InternalError (TyperCheckFAIL func))
           | x -> x
         in
         let carg = betaReduce' carg in
         betaReduce' (writeConvertion (App { func; carg })))
    | If { cond; tbranch; fbranch } ->
      (match cond.epre with
       | Const (Int 0) -> betaReduce' tbranch
       | Const (Int _) -> betaReduce' fbranch
       | _ ->
         (match betaReduce' cond with
          | x when x = cond -> raise (InternalError (TyperCheckFAIL cond))
          | cond -> betaReduce' (writeConvertion (If { cond; tbranch; fbranch }))))
    | Let { varg; init; body } ->
      let init = betaReduce' init in
      let body = betaReduce' (substitute varg.id init body) in
      betaReduce' body
    | Lambda _ | Const _ | Var _ -> expr
  in
  let e = alphaConverter e in
  try alphaReverser (betaReduce' e) with
  | InternalError (TyperCheckFAIL e) ->
    raise (EvalError { message = "Not a function"; location = e.epos })
  | InternalError (Unbound v) ->
    raise (EvalError { message = "Unbound variable"; location = v.vpos })
;;
