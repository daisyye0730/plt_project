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
  | LIST
  | NONE
  | STRING
  | RETURN
  | DEF
  | INT_LITERAL of (int)
  | FLOAT_LITERAL of (string)
  | CHAR_LITERAL of (char)
  | STRING_LITERAL of (string)
  | BLIT of (bool)
  | ID of (string)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
