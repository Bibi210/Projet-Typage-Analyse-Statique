open Ast
open Helpers
module Env = Map.Make (String)

type typing_errors =
  | Unification of equation
  | Unbound of variable

exception InternalError of typing_errors

exception
  TypingError of
    { message : string
    ; location : Helpers.position
    ; equation : string
    }

let occurCheck var typ =
  let rec occurCheck' var typ =
    match typ.tpre with
    | TVar x when x = var -> true
    | TLambda { targ; tbody } -> occurCheck' var targ || occurCheck' var tbody
    | _ -> false
  in
  occurCheck' var typ
;;

let rec substitute var newtyp oldtyp =
  match oldtyp.tpre with
  | TVar x when x = var -> newtyp
  | TLambda { targ; tbody } ->
    let targ' = substitute var newtyp targ in
    let tbody' = substitute var newtyp tbody in
    { tpos = oldtyp.tpos; tpre = TLambda { targ = targ'; tbody = tbody' } }
  | _ -> oldtyp
;;

let substituteAll var typ equations =
  List.map
    (fun { left; right } ->
      { left = substitute var typ left; right = substitute var typ right })
    equations
;;

let generateTVar pos = { tpos = pos; tpre = TVar (symbolGenerator "tvar") }

let generateConstTypeEquations node pos target =
  match node with
  | Int _ -> [ { left = target; right = { tpos = pos; tpre = TConst TInt } } ]
;;

let rec generateTypeEquations tree =
  let rec generateEquation' node target env =
    (match node.etyp_annotation with
     | Some t -> [ { left = target; right = t } ]
     | None -> [])
    @
    match node.epre with
    | Var x ->
      [ (match Env.find_opt x.id env with
         | Some t -> { left = target; right = t }
         | None -> { left = target; right = raise (InternalError (Unbound x)) })
      ]
    | Lambda { varg; body } ->
      let targ = generateTVar node.epos in
      let tbody = generateTVar node.epos in
      let env' = Env.add varg.id targ env in
      let eq1 = generateEquation' body tbody env' in
      { left = target; right = { tpos = node.epos; tpre = TLambda { targ; tbody } } }
      :: eq1
    | App { func; carg } ->
      let targ = generateTVar node.epos in
      let eq1 =
        generateEquation'
          func
          { tpos = node.epos; tpre = TLambda { targ; tbody = target } }
          env
      in
      let eq2 = generateEquation' carg targ env in
      eq1 @ eq2
    | Const c -> generateConstTypeEquations c node.epos target
    | If { cond; tbranch; fbranch } ->
      let eq1 = generateEquation' cond { tpos = node.epos; tpre = TConst TInt } env in
      let eq2 = generateEquation' tbranch target env in
      let eq3 = generateEquation' fbranch target env in
      eq1 @ eq2 @ eq3
    | _ -> failwith "Not implemented"
  in
  generateEquation' tree (generateTVar tree.epos) Env.empty

and unify ls =
  match ls with
  | [] -> []
  | { left; right } :: tail ->
    (match left.tpre, right.tpre with
     | leftT, rightT when leftT = rightT -> unify tail
     | TVar x, _ when not (occurCheck x right) -> unify (substituteAll x right tail)
     | _, TVar x when not (occurCheck x left) -> unify (substituteAll x left tail)
     | TLambda { targ; tbody }, TLambda { targ = targ'; tbody = tbody' } ->
       unify ({ left = targ; right = targ' } :: { left = tbody; right = tbody' } :: tail)
     | _ -> raise (InternalError (Unification { left; right })))
;;

let infer tree =
  try
    let equations = generateTypeEquations tree in
    let _unified = unify equations in
    ()
  with
  | InternalError (Unification a) ->
    raise
      (TypingError
         { message = Prettyprinter.string_of_equation a
         ; location = a.left.tpos
         ; equation = Prettyprinter.string_of_equation_list (generateTypeEquations tree)
         })
  | InternalError (Unbound v) ->
    raise
      (TypingError
         { message = "Unbound variable " ^ v.id; location = v.vpos; equation = "" })
;;
