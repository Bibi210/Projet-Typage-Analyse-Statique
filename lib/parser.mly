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
%token <string> LBasicIdent LVarType

%token LSimpleArrow
%token LFun

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
    | v = variable ; { Var v }
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


    