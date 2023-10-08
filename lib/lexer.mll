{
  open Parser

    exception LexingError of {
    msg: string;
    pos: Lexing.position;
  }
}

let alphanum = ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let basic_ident = ['a'-'z' '_'] alphanum
let num = ('-')?['0'-'9']*
let bool = ("true"|"false")

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n" | "\n\r"

rule token = parse
| '(' {LOpenPar}
| ')' {LClosePar}
| "fun" {LFun}
| '\\' {LFun}
| "->" {LSimpleArrow}
| basic_ident as ident {LBasicIdent ident}
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