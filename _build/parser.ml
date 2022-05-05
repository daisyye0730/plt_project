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

open Parsing;;
let _ = parse_error;;
# 4 "parser.mly"
open Ast
# 60 "parser.ml"
let yytransl_const = [|
  257 (* SEMI *);
  258 (* LPAREN *);
  259 (* RPAREN *);
  260 (* LBRACE *);
  261 (* RBRACE *);
  262 (* LBRACKET *);
  263 (* RBRACKET *);
  264 (* COMMA *);
  265 (* COLON *);
  266 (* DOT *);
  267 (* PLUS *);
  268 (* MINUS *);
  269 (* TIMES *);
  270 (* DIVIDE *);
  271 (* MODULO *);
  272 (* PLUSPLUS *);
  273 (* MINUSMINUS *);
  274 (* NOT *);
  275 (* ASSIGN *);
  276 (* MINUSEQ *);
  277 (* PLUSEQ *);
  278 (* EQ *);
  279 (* NEQ *);
  280 (* LT *);
  281 (* LEQ *);
  282 (* GT *);
  283 (* GEQ *);
  284 (* AND *);
  285 (* OR *);
  286 (* BREAK *);
  287 (* CONTINUE *);
  288 (* IF *);
  289 (* ELSE *);
  290 (* ELIF *);
  291 (* FOR *);
  292 (* WHILE *);
  293 (* INT *);
  294 (* FLOAT *);
  295 (* BOOL *);
  296 (* CHAR *);
  297 (* LIST *);
  298 (* NONE *);
  299 (* STRING *);
  300 (* RETURN *);
  301 (* DEF *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  302 (* INT_LITERAL *);
  303 (* FLOAT_LITERAL *);
  304 (* CHAR_LITERAL *);
  305 (* STRING_LITERAL *);
  306 (* BLIT *);
  307 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\005\000\005\000\003\000\006\000\
\006\000\006\000\006\000\006\000\006\000\006\000\007\000\008\000\
\008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
\008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
\008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
\008\000\008\000\008\000\009\000\009\000\011\000\011\000\012\000\
\012\000\014\000\015\000\015\000\015\000\013\000\013\000\013\000\
\013\000\013\000\013\000\013\000\013\000\004\000\016\000\016\000\
\017\000\017\000\010\000\010\000\018\000\018\000\000\000"

let yylen = "\002\000\
\002\000\000\000\003\000\002\000\000\000\003\000\002\000\001\000\
\001\000\001\000\001\000\006\000\001\000\001\000\004\000\001\000\
\001\000\001\000\001\000\001\000\001\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\002\000\002\000\003\000\003\000\003\000\003\000\
\003\000\006\000\004\000\000\000\001\000\001\000\003\000\000\000\
\002\000\008\000\000\000\005\000\008\000\002\000\003\000\001\000\
\005\000\009\000\003\000\002\000\002\000\008\000\000\000\001\000\
\001\000\003\000\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\008\000\010\000\009\000\011\000\000\000\013\000\
\014\000\071\000\000\000\000\000\000\000\000\000\000\000\001\000\
\000\000\000\000\004\000\007\000\000\000\003\000\000\000\000\000\
\064\000\000\000\000\000\000\000\000\000\066\000\000\000\012\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\016\000\017\000\018\000\020\000\
\019\000\000\000\000\000\000\000\000\000\000\000\056\000\006\000\
\000\000\000\000\000\000\000\000\045\000\060\000\061\000\000\000\
\000\000\000\000\000\000\000\000\000\000\035\000\036\000\000\000\
\000\000\054\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\062\000\
\049\000\038\000\055\000\000\000\039\000\000\000\000\000\000\000\
\059\000\000\000\000\000\068\000\000\000\000\000\000\000\000\000\
\022\000\023\000\024\000\025\000\026\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\047\000\000\000\000\000\
\000\000\000\000\043\000\015\000\000\000\000\000\000\000\057\000\
\070\000\000\000\000\000\000\000\000\000\042\000\000\000\000\000\
\000\000\000\000\000\000\000\000\050\000\000\000\000\000\000\000\
\058\000\052\000\000\000\000\000\000\000\053\000"

let yydgoto = "\002\000\
\010\000\011\000\012\000\013\000\034\000\014\000\051\000\052\000\
\060\000\099\000\061\000\053\000\054\000\055\000\132\000\024\000\
\025\000\100\000"

let yysindex = "\013\000\
\164\000\000\000\000\000\000\000\000\000\000\000\015\255\000\000\
\000\000\000\000\024\000\010\255\164\000\230\254\164\000\000\000\
\164\000\164\000\000\000\000\000\045\255\000\000\046\255\052\255\
\000\000\011\255\164\000\054\255\059\255\000\000\164\000\000\000\
\062\255\079\255\164\000\020\255\079\255\020\255\081\255\087\255\
\089\255\090\255\096\255\020\255\000\000\000\000\000\000\000\000\
\000\000\255\254\083\255\178\255\095\255\079\255\000\000\000\000\
\115\000\107\255\017\001\106\255\000\000\000\000\000\000\020\255\
\020\255\020\255\197\255\020\255\074\255\000\000\000\000\020\255\
\020\255\000\000\020\255\020\255\020\255\020\255\020\255\020\255\
\020\255\020\255\020\255\020\255\020\255\020\255\020\255\000\000\
\000\000\000\000\000\000\020\255\000\000\143\000\216\255\170\000\
\000\000\039\001\118\255\000\000\253\254\083\255\058\001\058\001\
\000\000\000\000\000\000\000\000\000\000\092\255\092\255\135\255\
\135\255\135\255\135\255\137\000\077\001\000\000\079\255\020\255\
\079\255\020\255\000\000\000\000\076\255\098\255\235\255\000\000\
\000\000\117\255\132\255\005\255\020\255\000\000\020\255\101\255\
\079\255\197\000\224\000\134\255\000\000\079\255\079\255\020\255\
\000\000\000\000\251\000\079\255\136\255\000\000"

let yyrindex = "\000\000\
\140\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\140\000\000\000\000\000\000\000\
\140\000\148\255\000\000\000\000\000\000\000\000\158\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\029\255\000\000\
\000\000\160\255\029\255\000\000\160\255\156\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\130\255\000\000\000\000\000\000\160\255\000\000\000\000\
\000\000\000\000\157\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\165\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\166\255\000\000\000\000\000\000\159\255\086\255\019\000\
\000\000\000\000\000\000\000\000\000\000\082\000\085\000\008\000\
\016\000\045\000\053\000\093\000\116\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\019\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\252\255\248\255\000\000\141\000\160\000\105\000\220\255\
\000\000\000\000\086\000\222\255\209\255\000\000\000\000\000\000\
\153\000\072\000"

let yytablesize = 617
let yytable = "\057\000\
\068\000\059\000\058\000\124\000\069\000\125\000\136\000\067\000\
\019\000\023\000\017\000\018\000\022\000\001\000\070\000\071\000\
\015\000\072\000\023\000\089\000\051\000\036\000\033\000\016\000\
\020\000\038\000\033\000\094\000\095\000\096\000\005\000\098\000\
\005\000\005\000\005\000\103\000\104\000\137\000\105\000\106\000\
\107\000\108\000\109\000\110\000\111\000\112\000\113\000\114\000\
\115\000\116\000\117\000\051\000\026\000\027\000\028\000\059\000\
\029\000\031\000\005\000\005\000\005\000\032\000\035\000\005\000\
\005\000\045\000\046\000\047\000\048\000\049\000\050\000\126\000\
\005\000\128\000\005\000\005\000\005\000\005\000\005\000\005\000\
\036\000\062\000\037\000\127\000\038\000\098\000\037\000\063\000\
\037\000\141\000\064\000\065\000\037\000\037\000\145\000\146\000\
\138\000\066\000\139\000\088\000\149\000\073\000\075\000\076\000\
\077\000\078\000\079\000\147\000\039\000\040\000\041\000\091\000\
\093\000\042\000\043\000\082\000\083\000\084\000\085\000\101\000\
\123\000\130\000\044\000\134\000\045\000\046\000\047\000\048\000\
\049\000\050\000\021\000\131\000\021\000\135\000\140\000\144\000\
\021\000\021\000\150\000\002\000\021\000\021\000\021\000\021\000\
\021\000\075\000\076\000\077\000\078\000\079\000\063\000\021\000\
\021\000\021\000\021\000\021\000\021\000\021\000\021\000\040\000\
\065\000\040\000\044\000\046\000\048\000\040\000\040\000\067\000\
\069\000\040\000\040\000\040\000\040\000\040\000\021\000\056\000\
\102\000\118\000\074\000\030\000\040\000\040\000\040\000\040\000\
\040\000\040\000\040\000\040\000\075\000\076\000\077\000\078\000\
\079\000\129\000\000\000\000\000\000\000\097\000\000\000\080\000\
\081\000\082\000\083\000\084\000\085\000\086\000\087\000\075\000\
\076\000\077\000\078\000\079\000\000\000\000\000\000\000\000\000\
\120\000\000\000\080\000\081\000\082\000\083\000\084\000\085\000\
\086\000\087\000\075\000\076\000\077\000\078\000\079\000\000\000\
\000\000\000\000\000\000\133\000\000\000\080\000\081\000\082\000\
\083\000\084\000\085\000\086\000\087\000\075\000\076\000\077\000\
\078\000\079\000\000\000\000\000\000\000\000\000\000\000\000\000\
\080\000\081\000\082\000\083\000\084\000\085\000\086\000\087\000\
\029\000\000\000\029\000\000\000\000\000\000\000\029\000\029\000\
\033\000\000\000\033\000\041\000\000\000\041\000\033\000\033\000\
\000\000\041\000\041\000\000\000\000\000\029\000\029\000\029\000\
\029\000\029\000\029\000\029\000\029\000\033\000\033\000\033\000\
\033\000\033\000\033\000\033\000\033\000\030\000\000\000\030\000\
\000\000\000\000\000\000\030\000\030\000\034\000\000\000\034\000\
\000\000\000\000\000\000\034\000\034\000\000\000\000\000\000\000\
\000\000\000\000\030\000\030\000\030\000\030\000\030\000\030\000\
\030\000\030\000\034\000\034\000\034\000\034\000\034\000\034\000\
\034\000\034\000\027\000\000\000\027\000\028\000\000\000\028\000\
\027\000\027\000\000\000\028\000\028\000\031\000\000\000\031\000\
\000\000\000\000\000\000\031\000\031\000\000\000\000\000\027\000\
\027\000\000\000\028\000\028\000\000\000\027\000\027\000\000\000\
\028\000\028\000\000\000\000\000\032\000\090\000\032\000\000\000\
\031\000\031\000\032\000\032\000\000\000\075\000\076\000\077\000\
\078\000\079\000\000\000\000\000\000\000\000\000\000\000\000\000\
\080\000\081\000\082\000\083\000\084\000\085\000\086\000\087\000\
\032\000\119\000\000\000\075\000\076\000\077\000\078\000\079\000\
\000\000\075\000\076\000\077\000\078\000\079\000\080\000\081\000\
\082\000\083\000\084\000\085\000\080\000\081\000\082\000\083\000\
\084\000\085\000\086\000\087\000\121\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\075\000\076\000\077\000\078\000\
\079\000\000\000\000\000\000\000\000\000\000\000\000\000\080\000\
\081\000\082\000\083\000\084\000\085\000\086\000\087\000\142\000\
\003\000\004\000\005\000\006\000\007\000\008\000\009\000\075\000\
\076\000\077\000\078\000\079\000\000\000\000\000\000\000\000\000\
\000\000\000\000\080\000\081\000\082\000\083\000\084\000\085\000\
\086\000\087\000\143\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\075\000\076\000\077\000\078\000\079\000\000\000\
\000\000\000\000\000\000\000\000\000\000\080\000\081\000\082\000\
\083\000\084\000\085\000\086\000\087\000\148\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\075\000\076\000\077\000\
\078\000\079\000\000\000\000\000\000\000\000\000\000\000\000\000\
\080\000\081\000\082\000\083\000\084\000\085\000\086\000\087\000\
\092\000\000\000\000\000\075\000\076\000\077\000\078\000\079\000\
\000\000\000\000\000\000\000\000\000\000\000\000\080\000\081\000\
\082\000\083\000\084\000\085\000\086\000\087\000\122\000\000\000\
\000\000\075\000\076\000\077\000\078\000\079\000\000\000\000\000\
\000\000\000\000\000\000\000\000\080\000\081\000\082\000\083\000\
\084\000\085\000\086\000\087\000\075\000\076\000\077\000\078\000\
\079\000\000\000\000\000\000\000\000\000\000\000\000\000\080\000\
\081\000\082\000\083\000\084\000\085\000\086\000\087\000\075\000\
\076\000\077\000\078\000\079\000\000\000\000\000\000\000\000\000\
\000\000\000\000\080\000\081\000\082\000\083\000\084\000\085\000\
\086\000"

let yycheck = "\036\000\
\002\001\038\000\037\000\007\001\006\001\009\001\002\001\044\000\
\013\000\018\000\001\001\002\001\017\000\001\000\016\001\017\001\
\002\001\019\001\027\000\054\000\002\001\002\001\031\000\000\000\
\051\001\006\001\035\000\064\000\065\000\066\000\002\001\068\000\
\004\001\005\001\006\001\072\000\073\000\033\001\075\000\076\000\
\077\000\078\000\079\000\080\000\081\000\082\000\083\000\084\000\
\085\000\086\000\087\000\033\001\008\001\008\001\003\001\092\000\
\046\001\004\001\030\001\031\001\032\001\003\001\001\001\035\001\
\036\001\046\001\047\001\048\001\049\001\050\001\051\001\119\000\
\044\001\121\000\046\001\047\001\048\001\049\001\050\001\051\001\
\002\001\001\001\004\001\120\000\006\001\122\000\001\001\001\001\
\003\001\137\000\002\001\002\001\007\001\008\001\142\000\143\000\
\133\000\002\001\135\000\005\001\148\000\019\001\011\001\012\001\
\013\001\014\001\015\001\144\000\030\001\031\001\032\001\005\001\
\007\001\035\001\036\001\024\001\025\001\026\001\027\001\046\001\
\003\001\046\001\044\001\007\001\046\001\047\001\048\001\049\001\
\050\001\051\001\001\001\034\001\003\001\002\001\034\001\002\001\
\007\001\008\001\003\001\000\000\011\001\012\001\013\001\014\001\
\015\001\011\001\012\001\013\001\014\001\015\001\003\001\022\001\
\023\001\024\001\025\001\026\001\027\001\028\001\029\001\001\001\
\003\001\003\001\007\001\007\001\005\001\007\001\008\001\003\001\
\003\001\011\001\012\001\013\001\014\001\015\001\015\000\035\000\
\072\000\092\000\001\001\027\000\022\001\023\001\024\001\025\001\
\026\001\027\001\028\001\029\001\011\001\012\001\013\001\014\001\
\015\001\122\000\255\255\255\255\255\255\001\001\255\255\022\001\
\023\001\024\001\025\001\026\001\027\001\028\001\029\001\011\001\
\012\001\013\001\014\001\015\001\255\255\255\255\255\255\255\255\
\001\001\255\255\022\001\023\001\024\001\025\001\026\001\027\001\
\028\001\029\001\011\001\012\001\013\001\014\001\015\001\255\255\
\255\255\255\255\255\255\001\001\255\255\022\001\023\001\024\001\
\025\001\026\001\027\001\028\001\029\001\011\001\012\001\013\001\
\014\001\015\001\255\255\255\255\255\255\255\255\255\255\255\255\
\022\001\023\001\024\001\025\001\026\001\027\001\028\001\029\001\
\001\001\255\255\003\001\255\255\255\255\255\255\007\001\008\001\
\001\001\255\255\003\001\001\001\255\255\003\001\007\001\008\001\
\255\255\007\001\008\001\255\255\255\255\022\001\023\001\024\001\
\025\001\026\001\027\001\028\001\029\001\022\001\023\001\024\001\
\025\001\026\001\027\001\028\001\029\001\001\001\255\255\003\001\
\255\255\255\255\255\255\007\001\008\001\001\001\255\255\003\001\
\255\255\255\255\255\255\007\001\008\001\255\255\255\255\255\255\
\255\255\255\255\022\001\023\001\024\001\025\001\026\001\027\001\
\028\001\029\001\022\001\023\001\024\001\025\001\026\001\027\001\
\028\001\029\001\001\001\255\255\003\001\001\001\255\255\003\001\
\007\001\008\001\255\255\007\001\008\001\001\001\255\255\003\001\
\255\255\255\255\255\255\007\001\008\001\255\255\255\255\022\001\
\023\001\255\255\022\001\023\001\255\255\028\001\029\001\255\255\
\028\001\029\001\255\255\255\255\001\001\003\001\003\001\255\255\
\028\001\029\001\007\001\008\001\255\255\011\001\012\001\013\001\
\014\001\015\001\255\255\255\255\255\255\255\255\255\255\255\255\
\022\001\023\001\024\001\025\001\026\001\027\001\028\001\029\001\
\029\001\003\001\255\255\011\001\012\001\013\001\014\001\015\001\
\255\255\011\001\012\001\013\001\014\001\015\001\022\001\023\001\
\024\001\025\001\026\001\027\001\022\001\023\001\024\001\025\001\
\026\001\027\001\028\001\029\001\003\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\011\001\012\001\013\001\014\001\
\015\001\255\255\255\255\255\255\255\255\255\255\255\255\022\001\
\023\001\024\001\025\001\026\001\027\001\028\001\029\001\003\001\
\037\001\038\001\039\001\040\001\041\001\042\001\043\001\011\001\
\012\001\013\001\014\001\015\001\255\255\255\255\255\255\255\255\
\255\255\255\255\022\001\023\001\024\001\025\001\026\001\027\001\
\028\001\029\001\003\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\011\001\012\001\013\001\014\001\015\001\255\255\
\255\255\255\255\255\255\255\255\255\255\022\001\023\001\024\001\
\025\001\026\001\027\001\028\001\029\001\003\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\011\001\012\001\013\001\
\014\001\015\001\255\255\255\255\255\255\255\255\255\255\255\255\
\022\001\023\001\024\001\025\001\026\001\027\001\028\001\029\001\
\008\001\255\255\255\255\011\001\012\001\013\001\014\001\015\001\
\255\255\255\255\255\255\255\255\255\255\255\255\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\029\001\008\001\255\255\
\255\255\011\001\012\001\013\001\014\001\015\001\255\255\255\255\
\255\255\255\255\255\255\255\255\022\001\023\001\024\001\025\001\
\026\001\027\001\028\001\029\001\011\001\012\001\013\001\014\001\
\015\001\255\255\255\255\255\255\255\255\255\255\255\255\022\001\
\023\001\024\001\025\001\026\001\027\001\028\001\029\001\011\001\
\012\001\013\001\014\001\015\001\255\255\255\255\255\255\255\255\
\255\255\255\255\022\001\023\001\024\001\025\001\026\001\027\001\
\028\001"

let yynames_const = "\
  SEMI\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  LBRACKET\000\
  RBRACKET\000\
  COMMA\000\
  COLON\000\
  DOT\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  MODULO\000\
  PLUSPLUS\000\
  MINUSMINUS\000\
  NOT\000\
  ASSIGN\000\
  MINUSEQ\000\
  PLUSEQ\000\
  EQ\000\
  NEQ\000\
  LT\000\
  LEQ\000\
  GT\000\
  GEQ\000\
  AND\000\
  OR\000\
  BREAK\000\
  CONTINUE\000\
  IF\000\
  ELSE\000\
  ELIF\000\
  FOR\000\
  WHILE\000\
  INT\000\
  FLOAT\000\
  BOOL\000\
  CHAR\000\
  LIST\000\
  NONE\000\
  STRING\000\
  RETURN\000\
  DEF\000\
  EOF\000\
  "

let yynames_block = "\
  INT_LITERAL\000\
  FLOAT_LITERAL\000\
  CHAR_LITERAL\000\
  STRING_LITERAL\000\
  BLIT\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    Obj.repr(
# 37 "parser.mly"
            ( _1)
# 440 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 40 "parser.mly"
                 ( ([], [])               )
# 446 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'decls) in
    Obj.repr(
# 41 "parser.mly"
                    ( ((_1 :: fst _3), snd _3) )
# 454 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'fdecl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decls) in
    Obj.repr(
# 42 "parser.mly"
               ( (fst _2, (_1 :: snd _2)) )
# 462 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    Obj.repr(
# 45 "parser.mly"
              ( [] )
# 468 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl_list) in
    Obj.repr(
# 46 "parser.mly"
                           (  _1 :: _3 )
# 476 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 49 "parser.mly"
         ( (_1, _2) )
# 484 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 52 "parser.mly"
          ( Int   )
# 490 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 53 "parser.mly"
          ( Bool  )
# 496 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 54 "parser.mly"
           ( Float )
# 502 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 55 "parser.mly"
           ( Char )
# 508 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'typ) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 57 "parser.mly"
                                              ( List(_3, _5) )
# 516 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 58 "parser.mly"
          ( None )
# 522 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "parser.mly"
           ( String )
# 528 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 62 "parser.mly"
                                     ( Access(_1, _3) )
# 536 "parser.ml"
               : 'list_index))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 65 "parser.mly"
                     ( Int_Literal(_1)        )
# 543 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 66 "parser.mly"
                     ( Float_Literal(_1)      )
# 550 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : char) in
    Obj.repr(
# 67 "parser.mly"
                     ( Char_Literal(_1)       )
# 557 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 68 "parser.mly"
                     ( BoolLit(_1)            )
# 564 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 69 "parser.mly"
                     ( String_Literal(_1)     )
# 571 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 70 "parser.mly"
                     ( Id(_1)                 )
# 578 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 71 "parser.mly"
                     ( Binop(_1, Add,   _3)   )
# 586 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 72 "parser.mly"
                     ( Binop(_1, Sub,   _3)   )
# 594 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 73 "parser.mly"
                     ( Binop(_1, Times, _3)   )
# 602 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 74 "parser.mly"
                     ( Binop(_1, Divide, _3)  )
# 610 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 75 "parser.mly"
                     ( Binop(_1, Modulo, _3)  )
# 618 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 76 "parser.mly"
                     ( Binop(_1, Equal, _3)   )
# 626 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 77 "parser.mly"
                     ( Binop(_1, Neq, _3)     )
# 634 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 78 "parser.mly"
                     ( Binop(_1, Less,  _3)   )
# 642 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 79 "parser.mly"
                     ( Binop(_1, Greater,_3)  )
# 650 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 80 "parser.mly"
                     ( Binop(_1, And,   _3)   )
# 658 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 81 "parser.mly"
                     ( Binop(_1, Or,    _3)   )
# 666 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 82 "parser.mly"
                     ( Binop(_1, Leq,   _3)   )
# 674 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 83 "parser.mly"
                     ( Binop(_1, Geq,   _3)   )
# 682 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 84 "parser.mly"
                   ( Assign(_1, Binop(Id(_1), Add, Int_Literal(1))) )
# 689 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 85 "parser.mly"
                   ( Assign(_1, Binop(Id(_1), Sub, Int_Literal(1))) )
# 696 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 86 "parser.mly"
                   ( Assign(_1, _3)         )
# 704 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 87 "parser.mly"
                       ( _2                   )
# 711 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'content_opt) in
    Obj.repr(
# 89 "parser.mly"
                                  (  ListLit(_2)      )
# 718 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'list_index) in
    Obj.repr(
# 90 "parser.mly"
                         ( Assign(_1, _3) )
# 726 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'list_index) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 91 "parser.mly"
                           ( ListAssign(_1, _3) )
# 734 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 92 "parser.mly"
                                                       ( Slice(_1, _3, _5) )
# 743 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 94 "parser.mly"
                              ( Call (_1, _3)  )
# 751 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 97 "parser.mly"
              ( [] )
# 757 "parser.ml"
               : 'content_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'content_list) in
    Obj.repr(
# 98 "parser.mly"
                 ( _1 )
# 764 "parser.ml"
               : 'content_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 101 "parser.mly"
       ([_1])
# 771 "parser.ml"
               : 'content_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'content_list) in
    Obj.repr(
# 102 "parser.mly"
                            (_1::_3)
# 779 "parser.ml"
               : 'content_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 105 "parser.mly"
  ( [] )
# 785 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 106 "parser.mly"
                   ( _1 :: _2 )
# 793 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : 'stmt) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'elif_stmt) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 110 "parser.mly"
        ( If ({if_branch= (_3, _5); elif_branch=List.rev _6; else_branch=_8}) )
# 803 "parser.ml"
               : 'if_stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 113 "parser.mly"
    ( [] )
# 809 "parser.ml"
               : 'elif_stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 114 "parser.mly"
                                 ( [(_3 , _5)] )
# 817 "parser.ml"
               : 'elif_stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : 'elif_stmt) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    Obj.repr(
# 115 "parser.mly"
                                                         ((_5 , _7) :: _1 )
# 826 "parser.ml"
               : 'elif_stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 118 "parser.mly"
                                            ( Expr _1      )
# 833 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 119 "parser.mly"
                                            ( Block _2 )
# 840 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'if_stmt) in
    Obj.repr(
# 120 "parser.mly"
                                            ( _1 )
# 847 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 121 "parser.mly"
                                            ( While (_3, _5)  )
# 855 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 122 "parser.mly"
                                                    ( For (_3, _5, _7, _9) )
# 865 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 123 "parser.mly"
                                            ( Return _2      )
# 872 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 124 "parser.mly"
                                            ( Break  )
# 878 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 125 "parser.mly"
                                            ( Continue )
# 884 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 131 "parser.mly"
  (
    {
      rtyp=fst _1;
      fname=snd _1;
      formals=_3;
      locals=_6;
      body=_7
    }
  )
# 902 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 143 "parser.mly"
              ( [] )
# 908 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 144 "parser.mly"
                 ( _1 )
# 915 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 147 "parser.mly"
        ( [_1] )
# 922 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 148 "parser.mly"
                             ( _1::_3 )
# 930 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 152 "parser.mly"
              ( [] )
# 936 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 153 "parser.mly"
         ( _1 )
# 943 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 156 "parser.mly"
        ( [_1] )
# 950 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 157 "parser.mly"
                    ( _1::_3 )
# 958 "parser.ml"
               : 'args))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
