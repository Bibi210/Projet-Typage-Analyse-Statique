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
  in
  substitute' expr
;;

let betaReduce e =
  let rec betaReduce' expr =
    let writeConvertion epre = { expr with epre } in
    match expr.epre with
    | App { func; carg } ->
      (match func.epre with
       | Lambda { varg; body } ->
         Prettyprinter.print_prog expr;
         betaReduce' (substitute body varg.id carg)
       | _ ->
         let func =
           match betaReduce' func with
           | x when x = func -> raise (InternalError (NotFunction func))
           | x -> x
         in
         let carg = betaReduce' carg in
         betaReduce' (writeConvertion (App { func; carg })))
    | _ -> expr
  in
  let e = alphaConverter e in
  try betaReduce' e with
  | InternalError (NotFunction e) ->
    raise (EvalError { message = "Not a function"; location = e.epos })
  | InternalError (Unbound v) ->
    raise (EvalError { message = "Unbound variable"; location = v.vpos })
;;
