(* Ocamllex scanner *)
{ open Parser }

let digit = ['0'-'9']
let float = (digit+) ['.'] digit+
let letter = ['a'-'z' 'A'-'Z']
let ascii = ([' '-'!' '#'-'[' ']'-'~'])
let char = ''' ( ascii | digit ) '''
let escape = []

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
| "and"    { AND } 
| "or"     { OR } 
| "in"     { IN }
(* Branch Control *)
| "if"     { IF }
| "else"   { ELSE }
| "elif"   { ELIF }
| "for"    { FOR }
| "while"  { WHILE }
| "continue" { CONTINUE }
| "break"  { BREAK }
| "within" { WITHIN }
(* Data Types *)
| "int"    { INT } 
| "float"  { FLOAT } 
| "bool"   { BOOL } 
| "char"   { CHAR } 
| "True"   { BLIT(true)  }
| "False"  { BLIT(false) }
| "class"  { CLASS } 
| "void"   { VOID }
| "List"   { LIST }
| "None"   { NONE }
(* Function *)
| "return" { RETURN }
| "def"    { DEF }
(* Other *)
| digit+ as lem  { INT_LITERAL(int_of_string lem) }
| float as lxm         { FLOAT_LITERAL(float_of_string lxm) }
| char as lxm          { CHAR_LITERAL( String.get lxm 1 ) }
| letter (digit | letter | '_')* as lem { ID(lem) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment_block = parse
  "###" { token lexbuf }
| _    { comment lexbuf }

and comment = parse 
  "\n" { token lexbuf }
| _    { comment lexbuf }