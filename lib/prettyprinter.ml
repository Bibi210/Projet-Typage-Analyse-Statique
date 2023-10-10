open Format
open Ast

let fmt_string = pp_print_string
let fmt_variable fmt { id; _ } = pp_print_string fmt id

let fmt_const fmt = function
  | Nat -> fmt_string fmt "Nat"
;;

let rec fmt_pre_type fmt ty =
  match ty.tpre with
  | TVar v -> fmt_string fmt v
  | TLambda x -> fprintf fmt "(%a -> %a)" fmt_pre_type x.varg fmt_pre_type x.tbody
  | TConst x -> fprintf fmt "%a" fmt_const x
  | TEquationEqual (left, right) ->
    fprintf fmt "(%a = %a)" fmt_pre_type left fmt_pre_type right
;;

let rec pre_type_to_string ty =
  match ty.tpre with
  | TVar v -> v
  | TLambda x ->
    "(" ^ pre_type_to_string x.varg ^ " -> " ^ pre_type_to_string x.tbody ^ ")"
  | TConst _ -> "Nat"
  | TEquationEqual (left, right) ->
    "(" ^ pre_type_to_string left ^ " = " ^ pre_type_to_string right ^ ")"
;;

let fmt_type fmt ty = fprintf fmt "%a" fmt_pre_type ty

let rec fmt_pre_expr fmt expr =
  match expr.epre with
  | Var v -> fmt_variable fmt v
  | App x -> fprintf fmt "(%a %a)" fmt_expr x.func fmt_expr x.carg
  | Lambda x -> fprintf fmt "(fun %a -> %a)" fmt_variable x.varg fmt_expr x.body

and fmt_expr fmt expr = fprintf fmt "(%a : %a)" fmt_pre_expr expr fmt_pre_type expr.etype

let rec nodeFmt_pre_expr fmt expr =
  match expr.epre with
  | Var v -> fprintf fmt "(Var %a)" fmt_variable v
  | App x -> fprintf fmt "(App%a,%a)" nodeFmt_pre_expr x.func nodeFmt_pre_expr x.carg
  | Lambda x ->
    fprintf fmt "(Lambda %a -> %a)" fmt_variable x.varg nodeFmt_pre_expr x.body
;;

let fmt_program fmt expr = fmt_expr fmt expr
let nodeFmt_program fmt expr = nodeFmt_pre_expr fmt expr
let fprintf_node_prog fmt prog = fprintf fmt "%a" nodeFmt_program prog

let fprintf_prog fmt prog =
  fprintf fmt "%a" fmt_program prog;
  fprintf fmt "\n"
;;
