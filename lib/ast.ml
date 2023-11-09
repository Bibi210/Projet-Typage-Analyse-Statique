type variable =
  { id : string
  ; vpos : Helpers.position
  }

type ctype =
  | TInt
  | TUnit
  | TLambda

type etype =
  { tpre : pre_type
  ; tpos : Helpers.position
  }
and constructors =
  | TRef
  | TLambda

and pre_type =
  | TVar of string
  | TConst of ctype
  | TAny of
      { id : string
      ; polytype : etype
      }
  | TApp of {
    constructor : constructors;
    args : etype array;
  }

type bin_op =
  | Add
  | Mul
  | Div
  | Mod
  | Lt
  | Gt
  | Or
  | Eq

type un_op =
  | Neg
  | Not

type expr =
  { epre : pre_expr
  ; epos : Helpers.position
  ; etyp_annotation : etype option
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
  | Var of string
  | If of
      { cond : expr
      ; tbranch : expr
      ; fbranch : expr
      }
  | Let of
      { varg : variable
      ; init : expr
      ; body : expr
      }
  | Fix of
      { varg : variable
      ; body : expr
      }
  | BinOp of
      { op : bin_op
      ; larg : expr
      ; rarg : expr
      }
  | UnOp of
      { op : un_op
      ; arg : expr
      }
  | Const of econst
  | Ref of expr
  | Deref of expr
  | Assign of
      { area : expr
      ; nval : expr
      }
  | Seq of
      { left : expr
      ; right : expr
      }

and econst = 
  | Int of int
  | Unit

type prog = expr

type equation =
  { left : etype
  ; right : etype
  }
