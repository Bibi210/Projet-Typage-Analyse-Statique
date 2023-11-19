type variable =
  { id : string
  ; vpos : Helpers.position
  }

type ctype =
  | TInt
  | TUnit
  | TRef
  | TLambda
  | TTuple

type etype =
  { tpre : pre_type
  ; tpos : Helpers.position
  }

and pre_type =
  | TVar of string
  | TConst of ctype
  | TAny of
      { id : string
      ; polytype : etype
      }
  | TApp of
      { constructor : etype
      ; args : etype array
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

and econst =
  | Int of int
  | Unit

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
  | Construct of
      { constructor : variable
      ; args : expr
      }
  | Tuple of expr array
  | Match of
      { matched : expr
      ; cases : match_case array
      }

and match_case =
  { pattern : pattern
  ; consequence : expr
  }

and pattern =
  { pnode : pre_pattern
  ; ppos : Helpers.position
  ; typAnnotation : etype option
  }

and pre_pattern =
  | LitteralPattern of econst
  | VarPattern of string
  | TuplePattern of pattern array
  | ConstructorPattern of
      { constructor_ident : string
      ; content : pattern
      }

type def =
  { basic_ident : string
  ; parameters : variable list
  ; constructors : newconstructor_case list
  ; dpos : Helpers.position
  }

and newconstructor_case =
  { constructor_ident : string
  ; content : etype array
  ; dcpos : Helpers.position
  }

type prog =
  { typedefs : def list
  ; e : expr
  }

type equation =
  { left : etype
  ; right : etype
  }
