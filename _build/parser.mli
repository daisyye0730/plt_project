type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACKET
  | RBRACKET
  | COMMA
  | COLON
  | DOT
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | MODULO
  | PLUSPLUS
  | MINUSMINUS
  | NOT
  | ASSIGN
  | MINUSEQ
  | PLUSEQ
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | AND
  | OR
  | IN
  | WITHIN
  | BREAK
  | CONTINUE
  | IF
  | ELSE
  | ELIF
  | FOR
  | WHILE
  | INT
  | FLOAT
  | BOOL
  | CHAR
  | CLASS
  | LIST
  | NONE
  | RETURN
  | DEF
  | INT_LITERAL of (int)
  | FLOAT_LITERAL of (float)
  | CHAR_LITERAL of (char)
  | BLIT of (bool)
  | ID of (string)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
