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

let rec occurCheck var typ =
  let fix = occurCheck var in
  match typ.tpre with
  | TVar x when x = var -> true
  | TLambda { targ; tbody } -> fix targ || fix tbody
  | TAny { polytype; _ } -> fix polytype
  | TConst _ | TVar _ -> false
;;

let rec substitute var newtyp oldtyp =
  let fix = substitute var newtyp in
  match oldtyp.tpre with
  | TVar x when x = var -> newtyp
  | TLambda { targ; tbody } ->
    { oldtyp with tpre = TLambda { targ = fix targ; tbody = fix tbody } }
  | TAny { id; polytype } ->
    let polytype' = fix polytype in
    { oldtyp with tpre = TAny { id; polytype = polytype' } }
  | TConst _ | TVar _ -> oldtyp
;;

let substituteAll var typ equations =
  List.map
    (fun { left; right } ->
      { left = substitute var typ left; right = substitute var typ right })
    equations
;;

let generateTVar str pos = { tpos = pos; tpre = TVar (symbolGenerator str) }
let generateAny id typ pos = { tpos = pos; tpre = TAny { id; polytype = typ } }
let targetString = symbolGenerator "target"
let unifyTarget progPos = { tpos = progPos; tpre = TVar targetString }

let rec renameTVar oldName newName typ =
  let fix = renameTVar oldName newName in
  { typ with
    tpre =
      (match typ.tpre with
       | TVar x when x = oldName -> TVar newName
       | TLambda { targ; tbody } -> TLambda { targ = fix targ; tbody = fix tbody }
       | TAny { id; polytype } -> TAny { id; polytype = fix polytype }
       | TConst _ | TVar _ -> typ.tpre)
  }
;;

let generateConstTypeEquations node pos target =
  { left = target
  ; right =
      { tpos = pos
      ; tpre =
          TConst
            (match node with
             | Int _ -> TInt)
      }
  }
;;

let rec generateTypeEquations tree target =
  let rec generateEquation' node target env =
    (match node.epre with
     | Var x ->
       [ (match Env.find_opt x env with
          | Some t -> { left = target; right = t }
          | None ->
            { left = target
            ; right = raise (InternalError (Unbound { id = x; vpos = node.epos }))
            })
       ]
     | Lambda { varg; body } ->
       let targ = generateTVar varg.id varg.vpos in
       let tbody = generateTVar "body" node.epos in
       let env' = Env.add varg.id targ env in
       let eq1 = generateEquation' body tbody env' in
       { left = target; right = { tpos = node.epos; tpre = TLambda { targ; tbody } } }
       :: eq1
     | App { func; carg } ->
       let targ = generateTVar "call" node.epos in
       let eq1 =
         generateEquation'
           func
           { tpos = node.epos; tpre = TLambda { targ; tbody = target } }
           env
       in
       let eq2 = generateEquation' carg targ env in
       eq2 @ eq1
     | Const c -> [ generateConstTypeEquations c node.epos target ]
     | If { cond; tbranch; fbranch } ->
       let eq1 = generateEquation' cond { tpos = node.epos; tpre = TConst TInt } env in
       let eq2 = generateEquation' tbranch target env in
       let eq3 = generateEquation' fbranch target env in
       eq1 @ eq2 @ eq3
     | Let { varg; init; body } ->
       let instancedType = infer init in
       let env' = Env.add varg.id (generateAny varg.id instancedType tree.epos) env in
       generateEquation' body target env')
    @
    match node.etyp_annotation with
    | Some t -> [ { left = t; right = target } ]
    | None -> []
  in
  generateEquation' tree target Env.empty

and unify ls =
  let rec unify' ls result =
    match ls with
    | [] -> result
    | { left; right } :: tail ->
      let result = { left; right } :: result in
      (match left.tpre, right.tpre with
       | TAny { id; polytype }, _ ->
         unify'
           ({ left = renameTVar id (symbolGenerator "instencied" ^ id) polytype; right }
            :: tail)
           result
       | _, TAny { id; polytype } ->
         unify'
           ({ left; right = renameTVar id (symbolGenerator "instencied" ^ id) polytype }
            :: tail)
           result
       | leftT, rightT when leftT = rightT -> unify' tail result
       | TVar x, _ when not (occurCheck x right) ->
         unify' (substituteAll x right tail) result
       | _, TVar x when not (occurCheck x left) ->
         unify' (substituteAll x left tail) result
       | TLambda { targ; tbody }, TLambda { targ = targ'; tbody = tbody' } ->
         unify'
           ({ left = targ; right = targ' } :: { left = tbody; right = tbody' } :: tail)
           result
       | _ -> raise (InternalError (Unification { left; right })))
  in
  let rec findResult result =
    match result with
    | [] ->
      raise
        (TypingError
           { message = "Cannot find type for %!target"
           ; location = Helpers.dummy_position
           ; equation = ""
           })
    | { left; right } :: tail ->
      (match left.tpre, right.tpre with
       | TVar x, _ when x = targetString -> right
       | _, TVar x when x = targetString -> left
       | TVar x, _ -> findResult (substituteAll x right tail)
       | _, TVar x -> findResult (substituteAll x left tail)
       | _ -> findResult tail)
  in
  findResult (unify' ls [])

and generateProgTypeEquation tree =
  let target = unifyTarget tree.epos in
  generateTypeEquations tree target

and infer tree =
  try unify (generateProgTypeEquation tree) with
  | InternalError (Unification a) ->
    raise
      (TypingError
         { message = Prettyprinter.string_of_equation a
         ; location = a.left.tpos
         ; equation =
             Prettyprinter.string_of_equation_list (generateProgTypeEquation tree)
         })
  | InternalError (Unbound v) ->
    raise
      (TypingError
         { message = "Unbound variable " ^ v.id; location = v.vpos; equation = "" })
;;
