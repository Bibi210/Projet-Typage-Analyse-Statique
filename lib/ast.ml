type variable =
  { id : string
  ; vpos : Helpers.position
  }

type ctype = Nat

type etype =
  { tpre : pre_type
  ; tpos : Helpers.position
  }

and pre_type =
  | TLambda of
      { varg : etype
      ; tbody : etype
      }
  | TVar of string
  | TConst of ctype
  | TEquationEqual of etype * etype

type expr =
  { epre : pre_expr
  ; epos : Helpers.position
  ; etype : etype
  }

and pre_expr =
  | App of
      { func : expr
      ; carg : expr
      }
  | Lambda of
      { varg : variable
      ; body : expr
      }
  | Var of variable

type prog = expr
