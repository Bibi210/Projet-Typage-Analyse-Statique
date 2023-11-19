%{
  open Ast
  open Helpers

  let position start endd = {
    start_pos = start;
    end_pos = endd;
    isSTD = false
  }
  let func_curryfy args body =
    List.fold_right
        (fun a acc ->
        { epre = Lambda { varg = a; body = acc }; epos = a.vpos; etyp_annotation = None })
        args
        body
    ;;

    let recfunc_curryfy var args body =
     {  epos = var.vpos;
        epre = Fix { varg = var; body = func_curryfy args body }
        ; etyp_annotation = None
      }
    ;;

    let functype_curryfy args body =
    List.fold_right
        (fun a acc -> { tpre =  TApp {constructor = { tpre = TConst TLambda; tpos = a.tpos } ; args = [a ; acc]} ; tpos = a.tpos })
        args
        body
    ;;

    let call_curryify func args =
    List.fold_left
        (fun acc a ->
        { epre = App { func = acc; carg = a }; epos = a.epos; etyp_annotation = None })
        func
        args
    ;;
%}
%token EOF
%token LOpenPar LClosePar LColon LRightAngleBraket LLeftAngleBraket
%token <int>Lint
%token <string> LBasicIdent  LConstructorIdent

%token LSimpleArrow LEqual
%token LFun LIf LThen LElse LLet LIn  LRec LSemiColon LRef LDeref  LOf LMatch LWith LUnderScore LQuote
%token LPut
%token LTupleInfixe 
%token LAdd LNeg LNot LMul LDiv LMod LOr LLess LGreater 
%token LType

%start <prog> prog
%%

prog:
    | typedefs = list(def); e = expr ; EOF { 
        { typedefs
        ; e 
        } 
}

def:
| LType; parameters = list(def_tvar) ; basic_ident = LBasicIdent ; LEqual ; 
  option(LOr) ;constructors = separated_nonempty_list(LOr,newconstructor_case){
    { dpos = position $startpos(basic_ident) $endpos(basic_ident);
      basic_ident; 
      parameters;
      constructors
    
  }
}

newconstructor_case:
| constructor_ident = LConstructorIdent{
  { constructor_ident
  ; content = [ {tpre = TConst TUnit ; tpos = position $startpos(constructor_ident) $endpos(constructor_ident) } ]
  ; dcpos = position $startpos(constructor_ident) $endpos(constructor_ident)
  }
}
| constructor_ident = LConstructorIdent; LOf ; etype = typing{
  { constructor_ident
  ; content = [etype]
  ; dcpos = position $startpos(constructor_ident) $endpos(constructor_ident)
  }
}
    

expr:
    | LOpenPar ; e = expr ; LClosePar { e }
    | e = pre_expr {
        {
            epre = e ; 
            epos = position $startpos(e) $endpos(e);
            etyp_annotation = None
        }
    }
    | LOpenPar ; epre = pre_expr ; LColon; etype = typing ; LClosePar {
        {
            epre;
            epos = position $startpos(epre) $endpos(epre);
            etyp_annotation = Some(etype) 
        }
    }

letbody:
    | LIn ; e = expr { e }
    | LSemiColon ; LSemiColon; e = expr { e }
    | e = expr { e }

pre_expr:
    | v = LBasicIdent { Var v }
    |  LFun  ; args = list(variable) ; LSimpleArrow ; body = expr   { 
        let args = if List.length args <> 0 then args 
          else [
                { id = symbolGenerator "unit";
                  vpos = position $startpos(args) $endpos(args) 
                }
          ] in
        (func_curryfy args body).epre
    }
    | LOpenPar ; func = expr ; args = nonempty_list(expr) ; LClosePar {
        (call_curryify func args).epre
    }
    | c = const { Const c }
    | LIf ; cond = expr ; LThen ; tbranch = expr ; LElse ; fbranch = expr {
            Match {
                matched = cond;
                cases = [
                    { pattern = { pnode = LitteralPattern (Int 0); ppos = position $startpos(cond) $endpos(cond); typAnnotation = None }; consequence = tbranch };
                    { pattern = { pnode = LitteralPattern (Int 1); ppos = position $startpos(cond) $endpos(cond); typAnnotation = None }; consequence = fbranch }
                ]
            }
    }
    | LLet; varg = variable; args = nonempty_list(variable);  LEqual; func_body = expr; content= letbody{
        Let {
            varg ; init = func_curryfy args func_body;body = content
        }
    }
    | LLet; LRec; varg = variable; args = nonempty_list(variable);  LEqual; func_body = expr; content= letbody{
        Let {
            varg ; init = recfunc_curryfy varg args func_body;body = content
        }
    }
    | LLet; varg = variable; LEqual; init = expr; body = letbody{
        Let {varg;init;body}
    }
    /* Generation de lambdas expr */
    | LOpenPar;op = unaryoperator ; arg = option(expr); LClosePar{
       match arg with
        | Some arg -> UnOp { op; arg }
        | None -> 
            let dummyvar = { id = symbolGenerator "x"; vpos = position $startpos(op) $endpos(op) } in
            let varexpr = {epre = Var dummyvar.id; epos = dummyvar.vpos; etyp_annotation = None} in
            let bodyexpr = {epre = UnOp { op; arg = varexpr}; epos = dummyvar.vpos; etyp_annotation = None} in
            Lambda { varg = dummyvar; body = bodyexpr }
    }/* Generation de lambdas expr */
    | LOpenPar ; larg = option(expr) ; op = binaryoperator ; rarg = option(expr);LClosePar {
        let dummyvar1 = { id = symbolGenerator "x"; vpos = position $startpos(op) $endpos(op) } in
        let dummyvar2 = { id = symbolGenerator "y"; vpos = position $startpos(op) $endpos(op) } in
        match larg,rarg with
        | Some larg, Some rarg -> BinOp { larg; op; rarg }
        | Some larg, None -> 
            let varexpr = {epre = Var dummyvar2.id; epos = dummyvar2.vpos; etyp_annotation = None} in
            let bodyexpr = {epre = BinOp { larg; op; rarg = varexpr}; epos = dummyvar2.vpos; etyp_annotation = None} in
            Lambda { varg = dummyvar2; body = bodyexpr }
        | None, Some rarg ->
            let varexpr = {epre = Var dummyvar1.id; epos = dummyvar1.vpos; etyp_annotation = None} in
            let bodyexpr = {epre = BinOp { larg = varexpr; op; rarg}; epos = dummyvar1.vpos; etyp_annotation = None} in
            Lambda { varg = dummyvar1; body = bodyexpr }
        | None, None ->
            let varexpr1 = {epre = Var dummyvar1.id; epos = dummyvar1.vpos; etyp_annotation = None} in
            let varexpr2 = {epre = Var dummyvar2.id; epos = dummyvar2.vpos; etyp_annotation = None} in
            let bodyexpr = {epre = BinOp { larg = varexpr1; op; rarg = varexpr2}; epos = dummyvar1.vpos; etyp_annotation = None} in
            let lambda1 = {epre = Lambda { varg = dummyvar2; body = bodyexpr } ; epos = dummyvar2.vpos; etyp_annotation = None} in
            Lambda { varg = dummyvar1; body = lambda1 }
    }
    | LRef; e = expr {
        Ref e 
    } 
    | LDeref; e = expr {
        Deref e
    }
    | LOpenPar;left = expr ; LSemiColon ; ls = separated_nonempty_list(LSemiColon,expr);LClosePar {
        let seq = List.fold_left (fun acc e -> {epre = Seq {left = acc; right = e}; epos = e.epos; etyp_annotation = None}) left ls in
        seq.epre
    }
    | LOpenPar; area = expr  ;  LPut ; nval = expr ; LClosePar {
        Assign {area; nval}
    }
    | t = tuple { t }
    | ident =  constructor; tuple = tuple {
         Construct { constructor =  ident ; args = {epre = tuple ; epos = position $startpos(tuple) $endpos(tuple); etyp_annotation = None} }
    }
    | ident =  constructor; LOpenPar ; expr = expr ; LClosePar {
         Construct { constructor =  ident ; args =  expr}
    }
    (*! SHIFTREDUCE *)
    | ident =  constructor  { 
         Construct { constructor =  ident ; args = {epre = Const Unit; epos = position $startpos(ident) $endpos(ident) ; etyp_annotation = None } }
    }
    | LMatch ; matched = expr ;LWith ;option(LOr) ; cases = separated_nonempty_list(LOr,match_case){
       Match{ matched  ; cases =  cases}  
    }

match_case :
| pattern = pattern ;LSimpleArrow; consequence = expr{
    { pattern ; consequence }
}

pattern:
    | LOpenPar ; p = pattern ; LClosePar { p }
    | p = pre_pattern {
        {
            pnode = p ; 
            ppos = position $startpos(p) $endpos(p);
            typAnnotation = None
        }
    }
    | LOpenPar ; ppre = pre_pattern ; LColon; etype = typing ; LClosePar {
        {
            pnode = ppre;
            ppos = position $startpos(ppre) $endpos(ppre);
            typAnnotation = Some(etype) 
        }
    }

pre_pattern:
| c = const {LitteralPattern c}
| v = LBasicIdent { VarPattern v }
| LUnderScore { VarPattern (symbolGeneratorIREV "wild") }
| t = tuplepattern { t }
| constructor_ident = LConstructorIdent {
  ConstructorPattern
      { constructor_ident
      ; content = {
            pnode = LitteralPattern Unit
            ; ppos = position $startpos(constructor_ident) $endpos(constructor_ident)
            ; typAnnotation = None
            }
      }
}
| constructor_ident = LConstructorIdent; content = pattern {
    ConstructorPattern
      { constructor_ident
      ; content 
      }
}

tuplepattern:
    | LOpenPar ; hd = pattern ; LTupleInfixe; tail = separated_nonempty_list(LTupleInfixe,pattern);LClosePar  {
        TuplePattern (hd::tail)
    }


tuple:
    | LOpenPar ; hd = expr ; LTupleInfixe; tail = separated_nonempty_list(LTupleInfixe,expr);LClosePar  {
        Tuple (hd::tail)
    }

const:
    | i = Lint { Int i }
    | LOpenPar; LClosePar { Unit }

variable:
    | var = LBasicIdent {
        {
            id = var; 
            vpos = position $startpos(var) $endpos(var)
        }
    }

def_tvar:
    | LQuote;var = LBasicIdent {
        {
            id = var; 
            vpos = position $startpos(var) $endpos(var)
        }
    }

constructor:
    | var = LConstructorIdent {
        {
            id = var; 
            vpos = position $startpos(var) $endpos(var)
        }
    }


%inline unaryoperator:
|LNeg {Neg}
|LNot {Not}


%inline binaryoperator:
|LAdd {Add}
|LEqual;LEqual {Eq}
|LMul {Mul}
|LMod {Mod}
|LLess {Lt}
|LGreater {Gt}
|LOr {Or}
|LDiv {Div}



typing:
    | LOpenPar; pre_type = pre_typing; LClosePar {
        {
            tpre = pre_type;
            tpos = position $startpos(pre_type) $endpos(pre_type)
        }
    } 
    | pre_type = pre_typing {
        {
            tpre = pre_type;
            tpos = position $startpos(pre_type) $endpos(pre_type)
        }
    } 

pre_typing:
    | var = def_tvar { TVar (var.id) }
    | basic_ident = LBasicIdent { 
        match basic_ident with
        | "int" -> TConst TInt
        | "unit" -> TConst TUnit
        | _ -> TVar basic_ident
        }
    | LRef ; { TConst TRef }
    | LOpenPar ; t1 = typing ; args = nonempty_list(typing) ; LClosePar {
        let args = t1::args in
        let rev_args = List.rev args in
        let last = List.hd rev_args in
        let remove_last = List.rev (List.tl rev_args) in
        TApp {constructor = last; args = remove_last}
    }
    | LOpenPar; args = nonempty_list(typing);LSimpleArrow;body = typing;LClosePar {
        (functype_curryfy args body).tpre
    }
    | LOpenPar ; hd = typing ; LMul ; tail = separated_nonempty_list(LMul,typing) ; LClosePar  {
        TApp {constructor = { tpre = TConst TTuple; tpos = position  $startpos($1) $endpos($5)  }; args = hd::tail}
    }




    