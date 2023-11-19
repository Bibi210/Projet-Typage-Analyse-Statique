open Ast
open Baselib
open Helpers
open TypingEnv

type typing_errors =
  | Unification of equation
  | Unbound of variable
  | UnboundConstructor of variable
  | AlreadyBound of variable

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
  | TAny { polytype; _ } -> fix polytype
  | TApp { constructor; args } -> Array.exists fix args || fix constructor
  | TConst _ | TVar _ -> false
;;

let rec substitute var newtyp oldtyp =
  let fix = substitute var newtyp in
  match oldtyp.tpre with
  | TVar x when x = var -> newtyp
  | TAny { id; polytype } -> { oldtyp with tpre = TAny { id; polytype = fix polytype } }
  | TApp { constructor; args } ->
    { oldtyp with
      tpre = TApp { constructor = fix constructor; args = Array.map fix args }
    }
  | TConst _ | TVar _ -> oldtyp
;;

let substituteAll var typ equations =
  List.map
    (fun { left; right } ->
      { left = substitute var typ left; right = substitute var typ right })
    equations
;;

let generateTVar str pos env =
  let symbol = symbolGenerator str in
  let typ = { tpos = pos; tpre = TVar symbol } in
  Env.add symbol typ env, typ
;;

let generateAny id typ pos = { tpos = pos; tpre = TAny { id; polytype = typ } }
let targetString () = symbolGenerator "target"
let unifyTarget progPos = { tpos = progPos; tpre = TVar (targetString ()) }

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

let showEnv env =
  Env.iter (fun x y -> Printf.printf "(%s -> %s)" x (Prettyprinter.string_of_type y)) env;
  print_endline ""
;;

let getTvar x =
  match x.tpre with
  | TVar x -> x
  | _ -> failwith "This is not a type variable"
;;

let addBoundVarToEnv typ env =
  let rec collectVars' typ excluded result =
    match typ.tpre with
    | TVar x when Env.mem x excluded -> result
    | TVar x -> Env.add x typ result
    | TConst _ -> result
    | TApp { args; _ } ->
      Array.fold_left (fun acc x -> collectVars' x excluded acc) result args
    | TAny { id; polytype } -> collectVars' polytype (Env.add id () excluded) result
  in
  collectVars' typ Env.empty env
;;

let generalise typ env =
  let rec generalise' typ env =
    match typ.tpre with
    | TVar x ->
      (match Env.find_opt x env with
       | Some _ -> [], env
       | None -> [ x ], Env.add x typ env)
    | TConst _ -> [], env
    | TApp { args; _ } ->
      Array.fold_left
        (fun (acc, env) x ->
          let acc', env' = generalise' x env in
          acc @ acc', env')
        ([], env)
        args
    | TAny _ -> failwith "Cannot on a non instencied type variable"
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

let rec generateEquation typeenv node target env =
  let generateEquation = generateEquation typeenv in
  (match node.etyp_annotation with
   | Some t -> [ { left = t; right = target } ]
   | None -> [])
  @
  match node.epre with
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
    let env, targ = generateTVar "" node.epos env in
    let eq1 = generateEquation x targ env in
    { left = target
    ; right =
        { tpos = node.epos
        ; tpre =
            TApp
              { constructor = { tpre = TConst TRef; tpos = node.epos }
              ; args = [| targ |]
              }
        }
    }
    :: eq1
  | Deref x ->
    let env, targ = generateTVar "access" node.epos env in
    let eq1 =
      generateEquation
        x
        { tpos = node.epos
        ; tpre =
            TApp
              { constructor = { tpre = TConst TRef; tpos = node.epos }
              ; args = [| targ |]
              }
        }
        env
    in
    { left = target; right = targ } :: eq1
  | Assign { area; nval } ->
    let env, tarea = generateTVar "area" node.epos env in
    let env, tnval = generateTVar "nval" node.epos env in
    let eq1 = generateEquation area tarea env in
    let eq2 = generateEquation nval tnval env in
    ({ left = { tpre = TConst TUnit; tpos = node.epos }; right = target }
     :: { left = tarea
        ; right =
            { tpre =
                TApp
                  { constructor = { tpre = TConst TRef; tpos = area.epos }
                  ; args = [| tnval |]
                  }
            ; tpos = tnval.tpos
            }
        }
     :: eq1)
    @ eq2
  | Lambda { varg; body } ->
    let env, targ = generateTVar varg.id node.epos env in
    let env, tbody = generateTVar "body" node.epos env in
    let env' = Env.add varg.id targ env in
    let eq1 = generateEquation body tbody env' in
    { left = target
    ; right =
        { tpos = node.epos
        ; tpre =
            TApp
              { constructor = { tpre = TConst TLambda; tpos = node.epos }
              ; args = [| targ; tbody |]
              }
        }
    }
    :: eq1
  | App { func; carg } ->
    let env, targ = generateTVar "call" node.epos env in
    let eq1 =
      generateEquation
        func
        { tpos = node.epos
        ; tpre =
            TApp
              { constructor = { tpre = TConst TLambda; tpos = func.epos }
              ; args = [| targ; target |]
              }
        }
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
    let instancedType, subs = infer' typeenv init env in
    let env =
      List.fold_left
        (fun env { left; right } ->
          match Env.find_opt (getTvar left) env with
          | Some _ -> addBoundVarToEnv right env
          | None -> env)
        env
        subs
    in
    let env' = Env.add varg.id (generalise instancedType env) env in
    let res = subs @ generateEquation body target env' in
    res
  | Fix { varg; body } ->
    let env, tbody = generateTVar "recbody" node.epos env in
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
  | Tuple args ->
    let env, targs =
      Array.fold_left
        (fun (env, ls) a ->
          let env, t = generateTVar "content" a.epos env in
          env, t :: ls)
        (env, [])
        args
    in
    let targs = Array.of_list targs in
    let args_equations = Array.map2 (fun a t -> generateEquation a t env) args targs in
    Array.fold_left (fun acc x -> acc @ x) [] args_equations
    @ [ { left = target
        ; right =
            { tpos = node.epos
            ; tpre =
                TApp
                  { constructor = { tpre = TConst TTuple; tpos = node.epos }
                  ; args = targs
                  }
            }
        }
      ]
  | Construct { constructor; args } ->
    (match Env.find_opt constructor.id typeenv with
     | Some def ->
       let constructor_content, owner = instanciateTypingEntry def node.epos in
       { left = target; right = owner }
       :: generateEquation args constructor_content.(0) env
     | None -> raise (InternalError (UnboundConstructor constructor)))
  | UnOp { op; arg } ->
    let op = find_unop op in
    let argtarget = { tpos = arg.epos; tpre = List.nth op.args_types 0 } in
    let eq1 = generateEquation arg argtarget env in
    [ { left = target; right = { tpos = node.epos; tpre = op.return_type } } ] @ eq1
  | Match { matched; cases } ->
    let env, tmatched = generateTVar "matched" node.epos env in
    let eq1 = generateEquation matched tmatched env in
    let eq2 =
      Array.map
        (fun { pattern; consequence } ->
          let env, tpattern = getPatternType typeenv pattern tmatched in
          tpattern @ generateEquation consequence target env)
        cases
    in
    eq1 @ Array.fold_left (fun acc x -> acc @ x) [] eq2

and getPatternType userEnv pattern target =
  let rec getPatternType pattern target typingenv =
    let makeExprEquiv pre_expr =
      { epos = pattern.ppos; epre = pre_expr; etyp_annotation = None }
    in
    match pattern.pnode with
    | LitteralPattern x ->
      typingenv, generateEquation userEnv (makeExprEquiv (Const x)) target typingenv
    | VarPattern x ->
      let env, t = generateTVar x pattern.ppos typingenv in
      (match Env.find_opt x userEnv with
       | None -> Env.add x t env, [ { left = target; right = t } ]
       | Some _ -> raise (InternalError (AlreadyBound { id = x; vpos = pattern.ppos })))
    | TuplePattern args ->
      let env, targs =
        Array.fold_left
          (fun (env, ls) a ->
            let env, t = generateTVar "pcontent" a.ppos env in
            env, t :: ls)
          (typingenv, [])
          args
      in
      List.fold_left2
        (fun (env, eqs) patt tvar ->
          let newenv, eq = getPatternType patt tvar env in
          newenv, eqs @ eq)
        (env, [])
        (List.of_seq (Array.to_seq args))
        targs
    | ConstructorPattern { constructor_ident; content } ->
      (match Env.find_opt constructor_ident userEnv with
       | Some def ->
         let constructor_content, owner = instanciateTypingEntry def pattern.ppos in
         let newenv, eq = getPatternType content constructor_content.(0) typingenv in
         newenv, { left = target; right = owner } :: eq
       | None ->
         raise
           (InternalError
              (UnboundConstructor { id = constructor_ident; vpos = pattern.ppos })))
  in
  getPatternType pattern target Env.empty

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
       | TApp { constructor; args }, TApp { constructor = constrb; args = argsb }
         when constructor.tpre = constrb.tpre ->
         if Array.length args <> Array.length argsb
         then raise (InternalError (Unification { left; right }))
         else (
           let equations =
             Array.to_list (Array.map2 (fun x y -> { left = x; right = y }) args argsb)
           in
           unify' (equations @ tail) result)
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

and infer' typeenv tree env =
  let target = unifyTarget tree.epos in
  try unify (generateEquation typeenv tree target env) target.tpre with
  | InternalError error ->
    (match error with
     | Unification a ->
       raise
         (TypingError
            { message = Prettyprinter.string_of_equation a
            ; location = a.left.tpos
            ; equation =
                Prettyprinter.string_of_equation_list
                  (generateEquation typeenv tree target env)
            })
     | Unbound v ->
       raise
         (TypingError
            { message = "Unbound variable " ^ v.id; location = v.vpos; equation = "" })
     | UnboundConstructor c ->
       raise
         (TypingError
            { message = "Unbound constructor " ^ c.id; location = c.vpos; equation = "" })
     | AlreadyBound v ->
       raise
         (TypingError
            { message = "Already bound variable in pattern " ^ v.id
            ; location = v.vpos
            ; equation = ""
            }))
;;

let removeInstance symbol =
  let index = String.index_opt symbol ':' in
  match index with
  | Some i ->
    let newSymbol = String.sub symbol i (String.length symbol - i) in
    String.sub newSymbol 1 (String.length newSymbol - 1)
  | None -> symbol
;;

let rec alphaReverser etype =
  { tpre =
      (match etype.tpre with
       | TVar x -> TVar (getNameFromSymbol (removeInstance x))
       | TConst _ -> etype.tpre
       | TApp { constructor; args } ->
         TApp { constructor; args = Array.map alphaReverser args }
       | TAny { id; polytype } -> TAny { id; polytype = alphaReverser polytype })
  ; tpos = etype.tpos
  }
;;

let infer tree =
  let typeenv, expr = createTypingEnv tree.typedefs, tree.e in
  let result, _ = infer' typeenv expr Env.empty in
  alphaReverser result
;;

let generateEquation tree =
  resetSymbolGenerator ();
  let _ = symbolGenerator "" in
  let typeenv, expr = createTypingEnv tree.typedefs, tree.e in
  generateEquation typeenv expr (unifyTarget expr.epos) Env.empty
;;
