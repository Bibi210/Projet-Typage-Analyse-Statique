open Ast
open Baselib
open Helpers
module Env = Map.Make (String)

type typing_errors =
  | Unification of equation
  | Unbound of variable
  | Substitution of etype

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
  | TRef x -> fix x
;;

let rec substitute var newtyp oldtyp =
  let fix = substitute var newtyp in
  match oldtyp.tpre with
  | TVar x when x = var -> newtyp
  | TLambda { targ; tbody } ->
    { oldtyp with tpre = TLambda { targ = fix targ; tbody = fix tbody } }
  | TAny { id; polytype } -> { oldtyp with tpre = TAny { id; polytype = fix polytype } }
  | TRef x -> { oldtyp with tpre = TRef (fix x) }
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
let targetString () = symbolGenerator "target"
let unifyTarget progPos = { tpos = progPos; tpre = TVar (targetString ()) }

let rec renameTVar oldName newName typ =
  let fix = renameTVar oldName newName in
  { typ with
    tpre =
      (match typ.tpre with
       | TVar x when x = oldName -> TVar newName
       | TLambda { targ; tbody } -> TLambda { targ = fix targ; tbody = fix tbody }
       | TAny { id; polytype } -> TAny { id; polytype = fix polytype }
       | TRef x -> TRef (fix x)
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
             | Int _ -> TInt
             | Unit -> TUnit)
      }
  }
;;

let generalise typ env =
  let rec generalise' typ env =
    match typ.tpre with
    | TVar x ->
      (match Env.find_opt x env with
       | Some _ -> [], env
       | None -> [ x ], Env.add x typ env)
    | TLambda { targ; tbody } ->
      let targ', env' = generalise' targ env in
      let tbody', env'' = generalise' tbody env' in
      targ' @ tbody', env''
    | TConst _ -> [], env
    | TRef x -> generalise' x env
    | TAny _ -> failwith "Cannot a non instencied  type variable"
  in
  let vars, _ = generalise' typ env in
  let polytype =
    List.fold_left
      (fun acc x -> { acc with tpre = TAny { id = x; polytype = acc } })
      typ
      vars
  in
  polytype
;;

let rec generateEquation node target env =
  (match node.epre with
   | Var x ->
     [ (match Env.find_opt x env with
        | Some t -> { left = target; right = t }
        | None -> raise (InternalError (Unbound { id = x; vpos = node.epos })))
     ]
   | Seq { left; right } ->
     let eq1 = generateEquation left { tpos = left.epos; tpre = TConst TUnit } env in
     let eq2 = generateEquation right target env in
     eq1 @ eq2
   | Ref x ->
     let targ = generateTVar "" node.epos in
     let eq1 = generateEquation x targ env in
     { left = target; right = { tpos = node.epos; tpre = TRef targ } } :: eq1
   | Deref x ->
     let targ = generateTVar "access" node.epos in
     let eq1 = generateEquation x { tpos = node.epos; tpre = TRef targ } env in
     { left = target; right = targ } :: eq1
   | Assign { area; nval } ->
     let tarea = generateTVar "area" node.epos in
     let tnval = generateTVar "nval" node.epos in
     let eq1 = generateEquation area tarea env in
     let eq2 = generateEquation nval tnval env in
     ({ left = { tpre = TConst TUnit; tpos = node.epos }; right = target }
      :: { left = tarea; right = { tpre = TRef tnval; tpos = tnval.tpos } }
      :: eq1)
     @ eq2
   | Lambda { varg; body } ->
     let targ = generateTVar varg.id varg.vpos in
     let tbody = generateTVar "body" node.epos in
     let env' = Env.add varg.id targ env in
     let eq1 = generateEquation body tbody env' in
     { left = target; right = { tpos = node.epos; tpre = TLambda { targ; tbody } } }
     :: eq1
   | App { func; carg } ->
     let targ = generateTVar "call" node.epos in
     let eq1 =
       generateEquation
         func
         { tpos = node.epos; tpre = TLambda { targ; tbody = target } }
         env
     in
     let eq2 = generateEquation carg targ env in
     eq2 @ eq1
   | Const c -> [ generateConstTypeEquations c node.epos target ]
   | If { cond; tbranch; fbranch } ->
     let eq1 = generateEquation cond { tpos = node.epos; tpre = TConst TInt } env in
     let eq2 = generateEquation tbranch target env in
     let eq3 = generateEquation fbranch target env in
     eq1 @ eq2 @ eq3
   | Let { varg; init; body } ->
     let instancedType, subs = infer' init env in
     let env' = Env.add varg.id (generalise instancedType env) env in
     let res = subs @ generateEquation body target env' in
     res
   | Fix { varg; body } ->
     let tbody = generateTVar "recbody" node.epos in
     let env' = Env.add varg.id tbody env in
     let eq1 = generateEquation body tbody env' in
     [ { left = target; right = tbody } ] @ eq1
   | BinOp { op; larg; rarg } ->
     let op = find_binop op in
     let largtarget = { tpos = larg.epos; tpre = List.nth op.args_types 0 } in
     let rargtarget = { tpos = rarg.epos; tpre = List.nth op.args_types 1 } in
     let eq1 = generateEquation larg largtarget env in
     let eq2 = generateEquation rarg rargtarget env in
     ({ left = target; right = { tpos = node.epos; tpre = op.return_type } } :: eq1) @ eq2
   | UnOp { op; arg } ->
     let op = find_unop op in
     let argtarget = { tpos = arg.epos; tpre = List.nth op.args_types 0 } in
     let eq1 = generateEquation arg argtarget env in
     [ { left = target; right = { tpos = node.epos; tpre = op.return_type } } ] @ eq1)
  @
  match node.etyp_annotation with
  | Some t -> [ { left = t; right = target } ]
  | None -> []

and unify ls target =
  let rec unify' ls result =
    match ls with
    | [] -> result
    | { left; right } :: tail ->
      (match left.tpre, right.tpre with
       | TAny { id; polytype }, _ ->
         unify'
           ({ left = renameTVar id (instanceGenerator id) polytype; right } :: tail)
           result
       | _, TAny { id; polytype } ->
         unify'
           ({ left; right = renameTVar id (instanceGenerator id) polytype } :: tail)
           result
       | leftT, rightT when leftT = rightT -> unify' tail result
       | TVar x, _ when not (occurCheck x right) ->
         unify' (substituteAll x right tail) ({ left; right } :: result)
       | _, TVar x when not (occurCheck x left) ->
         unify' (substituteAll x left tail) ({ right = left; left = right } :: result)
       | TLambda { targ; tbody }, TLambda { targ = targ'; tbody = tbody' } ->
         unify'
           ({ left = targ; right = targ' } :: { left = tbody; right = tbody' } :: tail)
           result
       | TRef x, TRef y -> unify' ({ left = x; right = y } :: tail) result
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
       | x, _ when x = target -> right
       | _, x when x = target -> left
       | TVar x, _ -> findResult (substituteAll x right tail)
       | _, TVar x -> findResult (substituteAll x left tail)
       | _ -> findResult tail)
  in
  let ls_subsitutions = unify' ls [] in
  findResult ls_subsitutions, ls_subsitutions

and infer' tree env =
  let target = unifyTarget tree.epos in
  try unify (generateEquation tree target env) target.tpre with
  | InternalError (Unification a) ->
    raise
      (TypingError
         { message = Prettyprinter.string_of_equation a
         ; location = a.left.tpos
         ; equation =
             Prettyprinter.string_of_equation_list (generateEquation tree target env)
         })
  | InternalError (Unbound v) ->
    raise
      (TypingError
         { message = "Unbound variable " ^ v.id; location = v.vpos; equation = "" })
;;

let infer tree =
  let result, _ = infer' tree Env.empty in
  result
;;

let generateEquation tree =
  resetSymbolGenerator ();
  let _ = symbolGenerator "" in
  generateEquation tree (unifyTarget tree.epos) Env.empty
;;
