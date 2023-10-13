open Ast
open Helpers
module Env = Map.Make (String)

type eval_errors =
  | NotFunction of expr
  | Unbound of variable

exception InternalError of eval_errors

exception
  EvalError of
    { message : string
    ; location : Helpers.position
    }

let alphaConverter expr =
  let changeVarId oldVar newid = { oldVar with id = newid } in
  let rec alphaConverter' expr env =
    let writeConvertion epre = { expr with epre } in
    match expr.epre with
    | Var x ->
      (match Env.find_opt x.id env with
       | Some new_id -> writeConvertion (Var (changeVarId x new_id))
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

let substitute expr id other =
  let rec substitute' expr =
    let writeConvertion epre = { expr with epre } in
    match expr.epre with
    | Var x -> if x.id = id then other else expr
    | Lambda { varg; body } -> writeConvertion (Lambda { varg; body = substitute' body })
    | App { func; carg } ->
      writeConvertion (App { func = substitute' func; carg = substitute' carg })
    | Const _ -> expr
    | If { cond; tbranch; fbranch } ->
      writeConvertion
        (If
           { cond = substitute' cond
           ; tbranch = substitute' tbranch
           ; fbranch = substitute' fbranch
           })
    | Let { varg; init; body } ->
      writeConvertion (Let { varg; init = substitute' init; body = substitute' body })
  in
  substitute' expr
;;

let betaReduce e =
  let rec betaReduce' expr =
    let writeConvertion epre = { expr with epre } in
    match expr.epre with
    | App { func; carg } ->
      (match func.epre with
       | Lambda { varg; body } -> betaReduce' (substitute body varg.id carg)
       | _ ->
         let func =
           match betaReduce' func with
           | x when x = func -> raise (InternalError (NotFunction func))
           | x -> x
         in
         let carg = betaReduce' carg in
         betaReduce' (writeConvertion (App { func; carg })))
    | If { cond; tbranch; fbranch } ->
      (match cond.epre with
       | Const (Int 0) -> betaReduce' tbranch
       | Const (Int 1) -> betaReduce' fbranch
       | _ ->
         let cond = betaReduce' cond in
         let tbranch = betaReduce' tbranch in
         let fbranch = betaReduce' fbranch in
         betaReduce' (writeConvertion (If { cond; tbranch; fbranch })))
    | _ -> expr
  in
  let e = alphaConverter e in
  try betaReduce' e with
  | InternalError (NotFunction e) ->
    raise (EvalError { message = "Not a function"; location = e.epos })
  | InternalError (Unbound v) ->
    raise (EvalError { message = "Unbound variable"; location = v.vpos })
;;
