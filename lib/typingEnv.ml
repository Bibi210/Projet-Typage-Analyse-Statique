open Ast
open Helpers

type typingEnvEntry =
  { constructor_content : etype list
  ; owner : pre_type
  ; vars : variable list
  }

let pretype_of_def (def : def) =
  match def.parameters with
  | [] -> TVar def.basic_ident
  | _ ->
    TApp
      { constructor = { tpre = TVar def.basic_ident; tpos = def.dpos }
      ; args = List.map (fun x -> { tpre = TVar x.id; tpos = x.vpos }) def.parameters
      }
;;

let createTypingEnv (def_ls : def list) =
  List.fold_left
    (fun acc x ->
      List.fold_left
        (fun acc elem ->
          Env.add
            elem.constructor_ident
            { constructor_content = elem.content
            ; owner = pretype_of_def x
            ; vars = x.parameters
            }
            acc)
        acc
        x.constructors)
    Env.empty
    def_ls
;;

let rec renameTVar oldName newName typ =
  let fix = renameTVar oldName newName in
  { typ with
    tpre =
      (match typ.tpre with
       | TVar x when x = oldName -> TVar newName
       | TAny { id; polytype } -> TAny { id; polytype = fix polytype }
       | TApp { constructor; args } ->
         TApp { constructor = fix constructor; args = List.map fix args }
       | TConst _ | TVar _ -> typ.tpre)
  }
;;

let instanciateTypingEntry { constructor_content; owner; vars } where =
  let renamedTVars = List.map (fun x -> x.id, symbolGenerator x.id) vars in
  let constructor_content =
    List.map
      (fun x ->
        List.fold_left (fun acc (old, newV) -> renameTVar old newV acc) x renamedTVars)
      constructor_content
  in
  let owner =
    List.fold_left
      (fun acc (old, newV) -> renameTVar old newV acc)
      { tpre = owner; tpos = where }
      renamedTVars
  in
  constructor_content, owner
;;
