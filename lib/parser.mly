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
        (fun a acc -> { tpre =  TApp {constructor = TLambda; args = [| a ; acc|]} ; tpos = a.tpos })
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
%token LOpenPar LClosePar LColon
%token <int>Lint
%token <string> LBasicIdent LVarType

%token LSimpleArrow LEqual
%token LFun LIf LThen LElse LLet LIn  LRec LSemiColon LRef LDeref 
%token LPut

%token LAdd LNeg LNot LMul LDiv LMod LOr LLess LGreater 


%token <Ast.pre_type>LParseType
%start <prog> prog
%%

prog:
    | p = expr ; EOF { p }
    

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
            If { cond; tbranch; fbranch };
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
    | var = LVarType { TVar var }
    | t = LParseType { t }
    | LOpenPar; args = nonempty_list(typing);LSimpleArrow;body = typing;LClosePar {
        (functype_curryfy args body).tpre
    }
    | LRef; t = typing {
        TApp {constructor = TRef; args = [|t|]}
    }


    