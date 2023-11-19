open Format
open Ast
open Lexing
open Helpers
open TypingEnv

let fmt_string = pp_print_string
let fmt_variable fmt { id; _ } = pp_print_string fmt id
let fmt_with_string str = pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt str)
let fmt_with_string_array str = pp_print_array ~pp_sep:(fun fmt () -> fprintf fmt str)
let fmt_with_space pp fmt l = fmt_with_string " " pp fmt l
let fmt_with_comma pp fmt l = fmt_with_string ", " pp fmt l
let fmt_variable_list = fmt_with_comma fmt_variable
let fmt_with_semicolon pp fmt l = fmt_with_string "; " pp fmt l
let fmt_with_mult pp fmt l = fmt_with_string "* " pp fmt l

let fmt_const fmt = function
  | TInt -> fmt_string fmt "int"
  | TUnit -> fmt_string fmt "unit"
  | TRef -> fmt_string fmt "ref"
  | TLambda -> fmt_string fmt "lambda"
  | TTuple -> fmt_string fmt "tuple"
;;

let rec fmt_pre_type fmt ty =
  match ty with
  | TVar v -> fmt_string fmt v
  | TConst x -> fprintf fmt "%a" fmt_const x
  | TAny x -> fprintf fmt "(any %a->%a)" fmt_string x.id fmt_type x.polytype
  | TApp x -> fprintf fmt "(%a %a)" fmt_type x.constructor fmt_type_array x.args

and fmt_type fmt ty = fprintf fmt "%a" fmt_pre_type ty.tpre
and fmt_type_array tyls = fmt_with_string_array "," fmt_type tyls
and fmt_type_list tyls = fmt_with_comma fmt_type tyls

let fmt_equation fmt { left; right } = fprintf fmt "%a = %a" fmt_type left fmt_type right
let fmt_equation_list = fmt_with_comma fmt_equation

let fmt_const_expr fmt expr =
  match expr with
  | Int n -> fprintf fmt "%d" n
  | Unit -> fmt_string fmt "()"
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

let rec fmt_pattern fmt patt =
  match patt.pnode with
  | LitteralPattern econst -> fprintf fmt "%a" fmt_const_expr econst
  | VarPattern string -> fprintf fmt "%s" string
  | TuplePattern patterns -> fprintf fmt "(%a)" fmt_pattern_array patterns
  | ConstructorPattern { constructor_ident : string; content : pattern } ->
    fprintf fmt "%s %a" constructor_ident fmt_pattern content

and fmt_pattern_array patts = fmt_with_string_array "," fmt_pattern patts

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
  | Seq x -> fprintf fmt "%a; %a" fmt_expr x.left fmt_expr x.right
  | Assign x -> fprintf fmt "%a := %a" fmt_expr x.area fmt_expr x.nval
  | Construct x -> fprintf fmt "%a(%a)" fmt_variable x.constructor fmt_expr x.args
  | Tuple x -> fprintf fmt "(%a)" fmt_expr_array x
  | Match x ->
    fprintf
      fmt
      "(match %a with \n %a)"
      fmt_expr
      x.matched
      (fmt_match_case_array typAnnot)
      x.cases

and fmt_match_case typAnnot fmt { pattern; consequence } =
  fprintf fmt "%a -> %a\n" fmt_pattern pattern (fmt_expr typAnnot) consequence

and fmt_match_case_array typAnnot casearr =
  fmt_with_string_array "|" (fmt_match_case typAnnot) casearr

and fmt_expr typAnnot fmt expr =
  let fmt_pre_expr = fmt_pre_expr typAnnot in
  match expr.etyp_annotation, typAnnot with
  | Some ty, true -> fprintf fmt "(%a : %a)" fmt_pre_expr expr.epre fmt_type ty
  | None, _ -> fprintf fmt "%a" fmt_pre_expr expr.epre
  | _, false -> fprintf fmt "%a" fmt_pre_expr expr.epre

and fmt_expr_array tyls = fmt_with_string_array "," (fmt_expr false) tyls

let fmt_expr_without_type fmt expr = fprintf fmt "%a" (fmt_expr false) expr
let fmt_expr_list typAnnot = fmt_with_comma (fmt_expr typAnnot)
let fmt_pre_expr_list = fmt_with_comma (fmt_pre_expr true)
let fmt_expr_list_without_type = fmt_with_comma fmt_expr_without_type

let fmt_memory fmt mem =
  let fmt_mem_entry fmt (id, expr) = fprintf fmt "%s : %a" id (fmt_expr false) expr in
  let fmt_mem_entry_list = fmt_with_comma fmt_mem_entry in
  fprintf fmt "{%a}" fmt_mem_entry_list (Env.bindings mem)
;;

let fmt_construtors fmt newConstr =
  fprintf
    fmt
    "@[%a of %a @]"
    fmt_string
    newConstr.constructor_ident
    (fmt_with_string_array "*" fmt_type)
    newConstr.content
;;

let fmt_construtors_list = fmt_with_string "\n|" fmt_construtors

let fmt_typedef fmt def =
  fprintf
    fmt
    "type %a %a = \n|%a"
    fmt_string
    def.basic_ident
    fmt_variable_list
    def.parameters
    fmt_construtors_list
    def.constructors
;;

let fmt_typingEnvEntry fmt (key, entry) =
  fprintf
    fmt
    "%a -> %a from %a"
    fmt_string
    key
    fmt_type_array
    entry.constructor_content
    fmt_pre_type
    entry.owner
;;

let fmt_typingEnv fmt env =
  let fmt_typingEnvEntry_list = fmt_with_semicolon fmt_typingEnvEntry in
  fprintf fmt "[%a]" fmt_typingEnvEntry_list (Env.bindings env)
;;

let print_typingEnv env =
  fmt_typingEnv Format.std_formatter env;
  Format.print_newline ()
;;

let fmt_typedef_list = fmt_with_string "\n" fmt_typedef

let print_typedef_list typedef_ls =
  fmt_typedef_list Format.std_formatter typedef_ls;
  Format.print_newline ()
;;

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

let string_of_type t =
  fmt_type Format.str_formatter t;
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

let print_evaluated_prog prog mem =
  Format.print_string "Memory = ";
  fmt_memory Format.std_formatter mem;
  Format.print_string "\n";
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
