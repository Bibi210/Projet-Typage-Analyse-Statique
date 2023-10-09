%{
  open Ast
  open Helpers

  let position start endd = {
  start_pos = start;
  end_pos = endd;
}
;;
%}
%token EOF
%token LOpenPar LClosePar LColon
%token <string> LBasicIdent LVarType

%token LSimpleArrow
%left LSimpleArrow
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
            etype = 
            {
                tpre = TVar (symbolGenerator "t") ;
                tpos = position $startpos(e) $endpos(e)
            }
        }
    }
    | LOpenPar ; epre = pre_expr ; LColon; etype = typing ; LClosePar {
        {
            epre;
            epos = position $startpos(epre) $endpos(epre);
            etype 
        }
    }
pre_expr:
    | v = variable ; { Var v }
    | LFun  ; varg = variable ; LSimpleArrow ; body = expr  { 
        Lambda { varg ; body }
    }
    | LOpenPar; func = expr ; carg = expr; LClosePar; { 
        App { func ; carg }
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
    | varg = typing; LSimpleArrow;  tbody = typing {
        TLambda {
            varg;
            tbody
        }
    }
    | t = LParseType { t }


    