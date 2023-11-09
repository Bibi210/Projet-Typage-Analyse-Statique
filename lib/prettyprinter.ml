open Format
open Ast
open Lexing
open Helpers

let fmt_string = pp_print_string
let fmt_variable fmt { id; _ } = pp_print_string fmt id
let fmt_with_string str = pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt str)
let fmt_with_space pp fmt l = fmt_with_string " " pp fmt l
let fmt_with_comma pp fmt l = fmt_with_string ", " pp fmt l
let fmt_with_semicolon pp fmt l = fmt_with_string "; " pp fmt l
let fmt_with_mult pp fmt l = fmt_with_string "* " pp fmt l

let fmt_const fmt = function
  | TInt -> fmt_string fmt "int"
;;

let rec fmt_pre_type fmt ty =
  match ty.tpre with
  | TVar v -> fmt_string fmt v
  | TLambda x -> fprintf fmt "(%a -> %a)" fmt_pre_type x.targ fmt_pre_type x.tbody
  | TConst x -> fprintf fmt "%a" fmt_const x
  | TAny x -> fprintf fmt "any %a %a" fmt_string x.id fmt_type x.polytype
  | TRef x -> fprintf fmt "ref %a" fmt_type x
  | TUnit -> fmt_string fmt "unit"

and fmt_type fmt ty = fprintf fmt "(%a)" fmt_pre_type ty

let fmt_equation fmt { left; right } = fprintf fmt "%a = %a" fmt_type left fmt_type right
let fmt_equation_list = fmt_with_comma fmt_equation

let fmt_const_expr fmt expr =
  match expr with
  | Int n -> fprintf fmt "%d" n
;;

let fmt_binary_operator fmt = function
  | Add -> fmt_string fmt "+"
  | Mul -> fmt_string fmt "*"
  | Div -> fmt_string fmt "/"
  | Eq -> fmt_string fmt "=="
  | Lt -> fmt_string fmt "<"
  | Gt -> fmt_string fmt ">"
  | Or -> fmt_string fmt "||"
  | Mod -> fmt_string fmt "%"
;;

let fmt_unary_operator fmt = function
  | Not -> fmt_string fmt "!"
  | Neg -> fmt_string fmt "-"
;;

let rec fmt_pre_expr typAnnot fmt expr =
  let fmt_expr = fmt_expr typAnnot in
  match expr with
  | Var v -> fmt_string fmt v
  | App x -> fprintf fmt "(%a %a)" fmt_expr x.func fmt_expr x.carg
  | Lambda x -> fprintf fmt "(fun %a -> %a)" fmt_variable x.varg fmt_expr x.body
  | Const x -> fprintf fmt "%a" fmt_const_expr x
  | If x ->
    fprintf
      fmt
      "if %a then %a else %a"
      fmt_expr
      x.cond
      fmt_expr
      x.tbranch
      fmt_expr
      x.fbranch
  | Let x ->
    fprintf fmt "let %a = %a in %a" fmt_variable x.varg fmt_expr x.init fmt_expr x.body
  | Fix x -> fprintf fmt "fix %a %a" fmt_variable x.varg fmt_expr x.body
  | UnOp { op; arg } -> fprintf fmt "%a %a" fmt_unary_operator op fmt_expr arg
  | BinOp { op; larg; rarg } ->
    fprintf fmt "%a %a %a" fmt_expr larg fmt_binary_operator op fmt_expr rarg
  | Ref x -> fprintf fmt "ref %a" fmt_expr x
  | Deref x -> fprintf fmt "!%a" fmt_expr x

and fmt_expr typAnnot fmt expr =
  let fmt_pre_expr = fmt_pre_expr typAnnot in
  match expr.etyp_annotation, typAnnot with
  | Some ty, true -> fprintf fmt "(%a : %a)" fmt_pre_expr expr.epre fmt_type ty
  | None, _ -> fprintf fmt "(%a)" fmt_pre_expr expr.epre
  | _, false -> fprintf fmt "(%a)" fmt_pre_expr expr.epre
;;

let fmt_expr_without_type fmt expr = fprintf fmt "%a" (fmt_expr false) expr
let fmt_expr_list typAnnot = fmt_with_comma (fmt_expr typAnnot)
let fmt_pre_expr_list = fmt_with_comma (fmt_pre_expr true)
let fmt_expr_list_without_type = fmt_with_comma fmt_expr_without_type

let fmt_error fmt msg pos =
  fprintf
    fmt
    "Error on line %d col %d: %s.\n"
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol)
    msg
;;

let print_error msg pos = fmt_error Format.err_formatter msg pos

let string_of_error msg pos =
  fmt_error Format.str_formatter msg pos;
  Format.flush_str_formatter ()
;;

let fmt_error_start_end fmt msg pos =
  fprintf
    fmt
    "Error from line %d col %d to line %d col %d: \n  %s.\n"
    pos.start_pos.pos_lnum
    (pos.start_pos.pos_cnum - pos.start_pos.pos_bol)
    pos.end_pos.pos_lnum
    (pos.end_pos.pos_cnum - pos.end_pos.pos_bol)
    msg
;;

let string_of_error_start_end msg pos =
  fmt_error_start_end Format.str_formatter msg pos;
  Format.flush_str_formatter ()
;;

let print_error_start_end msg pos = fmt_error_start_end Format.err_formatter msg pos

let string_of_expr expr =
  (fmt_expr false) Format.str_formatter expr;
  Format.flush_str_formatter ()
;;

let string_of_expr_with_annot expr =
  (fmt_expr true) Format.str_formatter expr;
  Format.flush_str_formatter ()
;;

let string_of_prog prog =
  fmt_expr_without_type Format.str_formatter prog;
  Format.flush_str_formatter ()
;;

let string_of_equation eq =
  fmt_equation Format.str_formatter eq;
  Format.flush_str_formatter ()
;;

let string_of_equation_list eqls =
  fmt_equation_list Format.str_formatter eqls;
  Format.flush_str_formatter ()
;;

let print_expr_list expr_ls =
  fmt_expr_list_without_type Format.std_formatter expr_ls;
  Format.print_newline ()
;;

let print_pre_expr_list expr_ls =
  fmt_pre_expr_list Format.std_formatter expr_ls;
  Format.print_newline ()
;;

let print_prog prog =
  fmt_expr_without_type Format.std_formatter prog;
  Format.print_newline ()
;;

let print_equation eq =
  fmt_equation Format.std_formatter eq;
  Format.print_newline ()
;;

let print_equation_list eq_ls =
  fmt_equation_list Format.std_formatter eq_ls;
  Format.print_newline ()
;;

let print_type ty =
  fmt_type Format.std_formatter ty;
  Format.print_newline ()
;;
