open Ast
open Helpers
module Env = Map.Make (String)

exception
  Type_error of
    { message : string
    ; location : Helpers.position
    }

let generateTVar pos = { tpos = pos; tpre = TVar (symbolGenerator "tvar") }
let unboudTVar pos = { tpos = pos; tpre = TVar (symbolGenerator "Unbound") }

let rec generateTypeEquations' target env tree =
  let keepOldPos oldType newt = { oldType with tpre = newt } in
  let treeTpos = tree.etype.tpos in
  let newType, newExpr =
    match tree.epre with
    | Var v ->
      ( (match Env.find_opt v.id env with
         | Some x -> keepOldPos tree.etype (TEquationEqual (x, target))
         | None -> keepOldPos tree.etype (TEquationEqual (unboudTVar treeTpos, target)))
      , tree.epre )
    | Lambda lmb ->
      let t_varArg = generateTVar treeTpos in
      let t_varbody = generateTVar lmb.body.etype.tpos in
      let rbody =
        generateTypeEquations' t_varbody (Env.add lmb.varg.id t_varArg env) lmb.body
      in
      let emitedType =
        keepOldPos tree.etype (TLambda { varg = t_varArg; tbody = t_varbody })
      in
      ( keepOldPos tree.etype (TEquationEqual (target, emitedType))
      , Lambda { lmb with body = rbody } )
    | App app ->
      let t_varApp = generateTVar treeTpos in
      let func =
        generateTypeEquations'
          (keepOldPos app.func.etype (TLambda { varg = t_varApp; tbody = target }))
          env
          app.func
      in
      let carg = generateTypeEquations' t_varApp env app.carg in
      tree.etype, App { func; carg }
  in
  { tree with
    etype = keepOldPos newType (TEquationEqual (tree.etype, newType))
  ; epre = newExpr
  }
;;

let generateTypeEquations tree =
  let t_var = generateTVar tree.etype.tpos in
  let rtree = generateTypeEquations' t_var Env.empty tree in
  rtree
;;

let rec occursCheck var t =
  match t.tpre with
  | TVar x when x = var -> true
  | TLambda { varg; tbody } -> occursCheck var varg || occursCheck var tbody
  | _ -> false
;;

let rec substitute typ tvar newtyp =
  match typ.tpre with
  | TVar x when x = tvar -> newtyp
  | TLambda { varg; tbody } ->
    let newvarg = substitute varg tvar newtyp in
    let newtbody = substitute tbody tvar newtyp in
    { typ with tpre = TLambda { varg = newvarg; tbody = newtbody } }
  | TEquationEqual (t1, t2) ->
    let newt1 = substitute t1 tvar newtyp in
    let newt2 = substitute t2 tvar newtyp in
    { typ with tpre = TEquationEqual (newt1, newt2) }
  | _ -> typ
;;

let rec substituteAndWrite tree tvar newtyp =
  let newType = substitute tree.etype tvar newtyp in
  let newExpr =
    match tree.epre with
    | Var _ -> tree.epre
    | Lambda lmb ->
      let newbody = substituteAndWrite lmb.body tvar newtyp in
      Lambda { lmb with body = newbody }
    | App app ->
      let newfunc = substituteAndWrite app.func tvar newtyp in
      let newcarg = substituteAndWrite app.carg tvar newtyp in
      App { func = newfunc; carg = newcarg }
  in
  { tree with etype = newType; epre = newExpr }
;;


(* TODO Passer a une liste d'équation type equation = ptype * ptype) list *)
(* TODO Change l'ast pour stock des Options equation *)
(* TODO Remove TEqEquation *)
(* TODO Fix Using Teacher stuff *)