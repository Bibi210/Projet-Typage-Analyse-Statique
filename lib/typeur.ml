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
