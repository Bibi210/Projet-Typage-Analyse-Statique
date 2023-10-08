open Format
open Ast

let fmt_string = pp_print_string
let fmt_variable fmt { id; _ } = fmt_string fmt id

let rec fmt_expr fmt expr =
  match expr.epre with
  | Var v -> fmt_variable fmt v
  | App x -> fprintf fmt "(%a %a)" fmt_expr x.func fmt_expr x.carg
  | Lambda x -> fprintf fmt "(fun %a -> %a)" fmt_variable x.varg fmt_expr x.body
;;

let rec nodeFmt_expr fmt expr =
  match expr.epre with
  | Var v -> fprintf fmt "(Var %a)" fmt_variable v
  | App x -> fprintf fmt "(App%a,%a)" nodeFmt_expr x.func nodeFmt_expr x.carg
  | Lambda x -> fprintf fmt "(Lambda %a -> %a)" fmt_variable x.varg nodeFmt_expr x.body
;;

let fmt_program fmt expr = fmt_expr fmt expr
let nodeFmt_program fmt expr = nodeFmt_expr fmt expr
let fprintf_node_prog fmt prog = fprintf fmt "%a" nodeFmt_program prog

let fprintf_prog fmt prog =
  fprintf fmt "%a" fmt_program prog;
  fprintf fmt "\n"
;;
