(* Ocamllex scanner *)
{ open Parser }

let digit = ['0'-'9']
let float = (digit+) ['.'] digit+
let letter = ['a'-'z' 'A'-'Z']
let ascii = ([' '-'!' '#'-'[' ']'-'~'])
let escape = '\\' ['\\' ''' '"' 'n' 'r' 't']
let char = ''' ( ascii | digit ) '''

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
(* Comments *)
| "#"      { comment lexbuf }           
| "###"    { comment_block lexbuf } 
(* Separators *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LBRACKET }
| ']'      { RBRACKET }
| ';'      { SEMI }
| ':'      { COLON }
| '.'      { DOT }
| ','      { COMMA }
(* Operators *)
| '+'      { PLUS } 
| '-'      { MINUS } 
| '*'      { TIMES } 
| '/'      { DIVIDE } 
| '%'      { MODULO } 
| '='      { ASSIGN } 
| "not"    { NOT }
| "=="     { EQ } 
| "!="     { NEQ } 
| '<'      { LT } 
| "<="     { LEQ } 
| ">"      { GT } 
| ">="     { GEQ } 
| "++"     { PLUSPLUS }
| "--"     { MINUSMINUS }
| "and"    { AND } 
| "or"     { OR } 
(* | "in"     { IN } *)
(* Branch Control *)
| "if"     { IF }
| "else"   { ELSE }
| "elif"   { ELIF }
| "for"    { FOR }
| "while"  { WHILE }
| "continue" { CONTINUE }
| "break"  { BREAK }
(* Data Types *)
| "int"    { INT } 
| "float"  { FLOAT } 
| "bool"   { BOOL } 
| "char"   { CHAR } 
| "string" { STRING }
| "true"   { BLIT(true)  }
| "false"  { BLIT(false) }
| "List"   { LIST }
| "None"   { NONE }
(* Function *)
| "return" { RETURN }
(* Other *)
| ['+' '-']?digit+ as lem  { INT_LITERAL(int_of_string lem) }
| ['0'-'9']+('.'['0'-'9']+)? | '.'['0'-'9']+ as lxm  { FLOAT_LITERAL(lxm) }
| char as lxm    { CHAR_LITERAL( String.get lxm 1 ) }
| '"' ( (ascii | escape)* as s) '"'   { STRING_LITERAL(s) }
| letter (digit | letter | '_')* as lem { ID(lem) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment_block = parse
  "###" { token lexbuf }
| _     { comment_block lexbuf }

and comment = parse 
  "\n" { token lexbuf }
| _    { comment lexbuf }