{
  open Parser

    exception LexingError of {
    msg: string;
    pos: Lexing.position;
  }

    let getToken t = 
    match t with
      |"nat" -> LParseType (TConst TNat)
      | "int" -> LParseType (TConst TInt)
      | a -> LBasicIdent a

}

let alphanum = ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let basic_ident = ['a'-'z' '_'] alphanum
let num = ('-')?['0'-'9']*
let bool = ("true"|"false")

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n" | "\n\r"
let vartype = [''']basic_ident*

rule token = parse
| '(' {LOpenPar}
| ')' {LClosePar}
| "fun" {LFun}
| '\\' {LFun}
| "->" {LSimpleArrow}
| ':' {LColon}
| "if" {LIf}
| "let" {LLet}
| "rec" {LRec}
| "=" {LEqual}
| "in" {LIn}
| "then" {LThen}
| "else" {LElse}
| vartype as v {LVarType v}
| basic_ident as ident {getToken ident}
| num+ as n       { Lint (int_of_string n) }
| "//"  { single_line_comment lexbuf }
| "(*" {multi_line_comment lexbuf}
| white* { token lexbuf }
| newline          { Lexing.new_line lexbuf; token lexbuf }
| eof {EOF}

and multi_line_comment = parse
| eof  { raise (LexingError {msg = "Never Ending Comment" ; pos = lexbuf.lex_curr_p}) }
| "*)" { token lexbuf }
| white* { multi_line_comment lexbuf }
| newline          { Lexing.new_line lexbuf; multi_line_comment lexbuf }
| _    { multi_line_comment lexbuf }

and single_line_comment = parse
| eof  { EOF }
| newline          { token lexbuf }
| _    { single_line_comment lexbuf }