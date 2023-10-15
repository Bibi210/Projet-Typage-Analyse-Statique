%{
  open Ast
  open Helpers

  let position start endd = {
    start_pos = start;
    end_pos = endd;
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
        (fun a acc -> { tpre = TLambda { targ = a; tbody = acc }; tpos = a.tpos })
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
%token LFun LIf LThen LElse LLet LIn  LRec


%token <Ast.pre_type>LParseType
%start <prog> prog
%%

prog:
    | expr ; EOF ; { $1 }
    

expr:
    | LOpenPar ; e = expr ; LClosePar ; { e }
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
pre_expr:
    | v = LBasicIdent ; { Var v }
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
    | LLet; varg = variable; args = nonempty_list(variable);  LEqual; func_body = expr; LIn ;content = expr{
        Let {
            varg ; init = func_curryfy args func_body;body = content
        }
    }
    | LLet; LRec; varg = variable; args = nonempty_list(variable);  LEqual; func_body = expr; LIn ;content = expr{
        Let {
            varg ; init = recfunc_curryfy varg args func_body;body = content
        }
    }
    | LLet; varg = variable; LEqual; init = expr; LIn ;body = expr{
        Let {varg;init;body}
    }

const:
    | i = Lint { Int i }

variable:
    | var = LBasicIdent {
        {
            id = var; 
            vpos = position $startpos(var) $endpos(var)
        }
    }

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


    