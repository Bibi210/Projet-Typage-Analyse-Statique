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
%token LOpenPar LClosePar 
%token <string> LBasicIdent 

%token LSimpleArrow
%token LFun

%start <prog> prog
%%

prog:
    | expr ; EOF ; { $1 }
    

expr:
    | e = pre_expr {
        {
            epre = e ; 
            epos = position $startpos(e) $endpos(e)
        }
    }
pre_expr:
    | LOpenPar ; e = pre_expr ; LClosePar ; { e }
    | v = variable ; { Var v }
    |LOpenPar; LFun  ; varg = variable ; LSimpleArrow ; body = expr; LClosePar  { 
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
    