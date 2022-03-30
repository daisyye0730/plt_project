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

open Parsing;;
let _ = parse_error;;
# 4 "parser.mly"
open Ast
# 61 "parser.ml"
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
  286 (* IN *);
  287 (* WITHIN *);
  288 (* BREAK *);
  289 (* CONTINUE *);
  290 (* IF *);
  291 (* ELSE *);
  292 (* ELIF *);
  293 (* FOR *);
  294 (* WHILE *);
  295 (* INT *);
  296 (* FLOAT *);
  297 (* BOOL *);
  298 (* CHAR *);
  299 (* CLASS *);
  300 (* LIST *);
  301 (* NONE *);
  302 (* RETURN *);
  303 (* DEF *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  304 (* INT_LITERAL *);
  305 (* FLOAT_LITERAL *);
  306 (* CHAR_LITERAL *);
  307 (* BLIT *);
  308 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\005\000\005\000\003\000\006\000\
\006\000\006\000\006\000\006\000\006\000\006\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\009\000\009\000\011\000\
\012\000\012\000\012\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\004\000\013\000\013\000\014\000\
\014\000\008\000\008\000\015\000\015\000\000\000"

let yylen = "\002\000\
\002\000\000\000\003\000\002\000\000\000\003\000\002\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\002\000\002\000\003\000\003\000\004\000\000\000\002\000\008\000\
\000\000\005\000\008\000\002\000\003\000\001\000\005\000\009\000\
\007\000\003\000\002\000\002\000\008\000\000\000\001\000\001\000\
\003\000\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\008\000\010\000\009\000\011\000\012\000\013\000\
\014\000\062\000\000\000\000\000\000\000\000\000\001\000\000\000\
\000\000\004\000\007\000\003\000\000\000\000\000\055\000\000\000\
\000\000\057\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\015\000\016\000\
\017\000\018\000\000\000\000\000\000\000\000\000\046\000\006\000\
\000\000\000\000\051\000\052\000\000\000\000\000\000\000\000\000\
\000\000\033\000\034\000\000\000\044\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\053\000\039\000\036\000\045\000\000\000\000\000\
\000\000\050\000\000\000\000\000\059\000\000\000\020\000\021\000\
\022\000\023\000\024\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\037\000\000\000\000\000\000\000\047\000\061\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\049\000\000\000\
\000\000\040\000\000\000\000\000\000\000\048\000\042\000\000\000\
\000\000\000\000\043\000"

let yydgoto = "\002\000\
\010\000\011\000\012\000\013\000\029\000\014\000\044\000\084\000\
\045\000\046\000\047\000\112\000\022\000\023\000\085\000"

let yysindex = "\011\000\
\035\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\021\000\008\255\035\255\229\254\000\000\035\255\
\035\255\000\000\000\000\000\000\039\255\049\255\000\000\035\255\
\050\255\000\000\035\255\052\255\053\255\035\255\017\255\053\255\
\055\255\058\255\070\255\087\255\090\255\017\255\000\000\000\000\
\000\000\000\000\011\255\042\000\091\255\053\255\000\000\000\000\
\107\000\093\255\000\000\000\000\017\255\017\255\017\255\061\000\
\017\255\000\000\000\000\017\255\000\000\017\255\017\255\017\255\
\017\255\017\255\017\255\017\255\017\255\017\255\017\255\017\255\
\017\255\017\255\000\000\000\000\000\000\000\000\134\000\134\255\
\161\000\000\000\035\001\094\255\000\000\054\001\000\000\000\000\
\000\000\000\000\000\000\100\000\100\000\246\254\246\254\246\254\
\246\254\128\255\073\001\053\255\017\255\017\255\053\255\017\255\
\000\000\071\255\080\000\188\000\000\000\000\000\108\255\013\255\
\017\255\053\255\017\255\076\255\053\255\215\000\000\000\242\000\
\112\255\000\000\053\255\053\255\017\255\000\000\000\000\013\001\
\053\255\118\255\000\000"

let yyrindex = "\000\000\
\115\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\115\000\000\000\000\000\115\000\
\119\255\000\000\000\000\000\000\121\255\000\000\000\000\000\000\
\000\000\000\000\012\255\000\000\120\255\012\255\000\000\120\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\105\255\000\000\000\000\120\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\123\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\135\255\000\000\000\000\092\255\000\000\000\000\
\000\000\000\000\000\000\241\255\245\255\163\255\173\255\202\255\
\212\255\249\255\080\255\000\000\000\000\000\000\000\000\000\000\
\000\000\016\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\251\255\153\000\000\000\114\000\000\000\225\255\000\000\
\230\255\179\000\000\000\000\000\000\000\113\000\063\000"

let yytablesize = 613
let yytable = "\049\000\
\062\000\063\000\064\000\065\000\066\000\050\000\056\000\018\000\
\016\000\017\000\020\000\001\000\057\000\005\000\116\000\005\000\
\005\000\041\000\031\000\076\000\015\000\079\000\080\000\081\000\
\019\000\083\000\058\000\059\000\086\000\060\000\087\000\088\000\
\089\000\090\000\091\000\092\000\093\000\094\000\095\000\096\000\
\097\000\098\000\099\000\005\000\005\000\005\000\024\000\117\000\
\005\000\005\000\041\000\025\000\030\000\027\000\031\000\051\000\
\032\000\005\000\052\000\005\000\005\000\005\000\005\000\005\000\
\039\000\040\000\041\000\042\000\043\000\107\000\108\000\053\000\
\083\000\003\000\004\000\005\000\006\000\007\000\008\000\009\000\
\030\000\118\000\030\000\120\000\033\000\034\000\035\000\030\000\
\054\000\036\000\037\000\055\000\035\000\128\000\035\000\075\000\
\105\000\078\000\038\000\035\000\039\000\040\000\041\000\042\000\
\043\000\019\000\111\000\019\000\030\000\115\000\030\000\121\000\
\019\000\125\000\002\000\019\000\019\000\019\000\019\000\019\000\
\131\000\054\000\035\000\056\000\038\000\058\000\019\000\019\000\
\019\000\019\000\019\000\019\000\019\000\019\000\101\000\019\000\
\026\000\060\000\062\000\063\000\064\000\065\000\066\000\048\000\
\062\000\063\000\064\000\065\000\066\000\067\000\068\000\069\000\
\070\000\071\000\072\000\067\000\068\000\069\000\070\000\071\000\
\072\000\073\000\074\000\027\000\102\000\027\000\110\000\000\000\
\000\000\021\000\027\000\000\000\000\000\031\000\000\000\031\000\
\021\000\000\000\000\000\028\000\031\000\000\000\028\000\000\000\
\027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
\000\000\027\000\031\000\031\000\031\000\031\000\031\000\031\000\
\031\000\031\000\028\000\031\000\028\000\000\000\000\000\000\000\
\000\000\028\000\000\000\000\000\032\000\000\000\032\000\000\000\
\000\000\000\000\000\000\032\000\000\000\000\000\000\000\028\000\
\028\000\028\000\028\000\028\000\028\000\028\000\028\000\000\000\
\028\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
\032\000\025\000\032\000\025\000\000\000\026\000\000\000\026\000\
\025\000\029\000\000\000\029\000\026\000\000\000\000\000\000\000\
\029\000\000\000\000\000\000\000\000\000\000\000\025\000\025\000\
\000\000\000\000\026\000\026\000\025\000\025\000\000\000\025\000\
\026\000\026\000\000\000\026\000\029\000\029\000\106\000\029\000\
\000\000\109\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\119\000\000\000\000\000\122\000\
\000\000\000\000\061\000\000\000\000\000\126\000\127\000\000\000\
\000\000\000\000\000\000\130\000\062\000\063\000\064\000\065\000\
\066\000\000\000\000\000\000\000\000\000\082\000\000\000\067\000\
\068\000\069\000\070\000\071\000\072\000\073\000\074\000\062\000\
\063\000\064\000\065\000\066\000\000\000\000\000\000\000\000\000\
\113\000\000\000\067\000\068\000\069\000\070\000\071\000\072\000\
\073\000\074\000\062\000\063\000\064\000\065\000\066\000\000\000\
\000\000\000\000\000\000\000\000\000\000\067\000\068\000\069\000\
\070\000\071\000\072\000\073\000\074\000\077\000\062\000\063\000\
\064\000\065\000\066\000\000\000\000\000\062\000\063\000\064\000\
\065\000\066\000\000\000\069\000\070\000\071\000\072\000\000\000\
\067\000\068\000\069\000\070\000\071\000\072\000\073\000\074\000\
\100\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\062\000\063\000\064\000\065\000\066\000\000\000\000\000\000\000\
\000\000\000\000\000\000\067\000\068\000\069\000\070\000\071\000\
\072\000\073\000\074\000\103\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\062\000\063\000\064\000\065\000\066\000\
\000\000\000\000\000\000\000\000\000\000\000\000\067\000\068\000\
\069\000\070\000\071\000\072\000\073\000\074\000\114\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\062\000\063\000\
\064\000\065\000\066\000\000\000\000\000\000\000\000\000\000\000\
\000\000\067\000\068\000\069\000\070\000\071\000\072\000\073\000\
\074\000\123\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\062\000\063\000\064\000\065\000\066\000\000\000\000\000\
\000\000\000\000\000\000\000\000\067\000\068\000\069\000\070\000\
\071\000\072\000\073\000\074\000\124\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\062\000\063\000\064\000\065\000\
\066\000\000\000\000\000\000\000\000\000\000\000\000\000\067\000\
\068\000\069\000\070\000\071\000\072\000\073\000\074\000\129\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\062\000\
\063\000\064\000\065\000\066\000\000\000\000\000\000\000\000\000\
\000\000\000\000\067\000\068\000\069\000\070\000\071\000\072\000\
\073\000\074\000\104\000\000\000\000\000\062\000\063\000\064\000\
\065\000\066\000\000\000\000\000\000\000\000\000\000\000\000\000\
\067\000\068\000\069\000\070\000\071\000\072\000\073\000\074\000\
\062\000\063\000\064\000\065\000\066\000\000\000\000\000\000\000\
\000\000\000\000\000\000\067\000\068\000\069\000\070\000\071\000\
\072\000\073\000\074\000\062\000\063\000\064\000\065\000\066\000\
\000\000\000\000\000\000\000\000\000\000\000\000\067\000\068\000\
\069\000\070\000\071\000\072\000\073\000"

let yycheck = "\031\000\
\011\001\012\001\013\001\014\001\015\001\032\000\038\000\013\000\
\001\001\002\001\016\000\001\000\002\001\002\001\002\001\004\001\
\005\001\002\001\002\001\046\000\000\000\053\000\054\000\055\000\
\052\001\057\000\016\001\017\001\060\000\019\001\062\000\063\000\
\064\000\065\000\066\000\067\000\068\000\069\000\070\000\071\000\
\072\000\073\000\074\000\032\001\033\001\034\001\008\001\035\001\
\037\001\038\001\035\001\003\001\001\001\004\001\002\001\001\001\
\004\001\046\001\001\001\048\001\049\001\050\001\051\001\052\001\
\048\001\049\001\050\001\051\001\052\001\101\000\102\000\002\001\
\104\000\039\001\040\001\041\001\042\001\043\001\044\001\045\001\
\001\001\113\000\003\001\115\000\032\001\033\001\034\001\008\001\
\002\001\037\001\038\001\002\001\001\001\125\000\003\001\005\001\
\003\001\005\001\046\001\008\001\048\001\049\001\050\001\051\001\
\052\001\001\001\036\001\003\001\029\001\002\001\031\001\036\001\
\008\001\002\001\000\000\011\001\012\001\013\001\014\001\015\001\
\003\001\003\001\031\001\003\001\005\001\003\001\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\029\001\001\001\031\001\
\024\000\003\001\011\001\012\001\013\001\014\001\015\001\030\000\
\011\001\012\001\013\001\014\001\015\001\022\001\023\001\024\001\
\025\001\026\001\027\001\022\001\023\001\024\001\025\001\026\001\
\027\001\028\001\029\001\001\001\031\001\003\001\104\000\255\255\
\255\255\017\000\008\001\255\255\255\255\001\001\255\255\003\001\
\024\000\255\255\255\255\027\000\008\001\255\255\030\000\255\255\
\022\001\023\001\024\001\025\001\026\001\027\001\028\001\029\001\
\255\255\031\001\022\001\023\001\024\001\025\001\026\001\027\001\
\028\001\029\001\001\001\031\001\003\001\255\255\255\255\255\255\
\255\255\008\001\255\255\255\255\001\001\255\255\003\001\255\255\
\255\255\255\255\255\255\008\001\255\255\255\255\255\255\022\001\
\023\001\024\001\025\001\026\001\027\001\028\001\029\001\255\255\
\031\001\022\001\023\001\024\001\025\001\026\001\027\001\028\001\
\029\001\001\001\031\001\003\001\255\255\001\001\255\255\003\001\
\008\001\001\001\255\255\003\001\008\001\255\255\255\255\255\255\
\008\001\255\255\255\255\255\255\255\255\255\255\022\001\023\001\
\255\255\255\255\022\001\023\001\028\001\029\001\255\255\031\001\
\028\001\029\001\255\255\031\001\028\001\029\001\100\000\031\001\
\255\255\103\000\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\114\000\255\255\255\255\117\000\
\255\255\255\255\001\001\255\255\255\255\123\000\124\000\255\255\
\255\255\255\255\255\255\129\000\011\001\012\001\013\001\014\001\
\015\001\255\255\255\255\255\255\255\255\001\001\255\255\022\001\
\023\001\024\001\025\001\026\001\027\001\028\001\029\001\011\001\
\012\001\013\001\014\001\015\001\255\255\255\255\255\255\255\255\
\001\001\255\255\022\001\023\001\024\001\025\001\026\001\027\001\
\028\001\029\001\011\001\012\001\013\001\014\001\015\001\255\255\
\255\255\255\255\255\255\255\255\255\255\022\001\023\001\024\001\
\025\001\026\001\027\001\028\001\029\001\003\001\011\001\012\001\
\013\001\014\001\015\001\255\255\255\255\011\001\012\001\013\001\
\014\001\015\001\255\255\024\001\025\001\026\001\027\001\255\255\
\022\001\023\001\024\001\025\001\026\001\027\001\028\001\029\001\
\003\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\011\001\012\001\013\001\014\001\015\001\255\255\255\255\255\255\
\255\255\255\255\255\255\022\001\023\001\024\001\025\001\026\001\
\027\001\028\001\029\001\003\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\011\001\012\001\013\001\014\001\015\001\
\255\255\255\255\255\255\255\255\255\255\255\255\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\029\001\003\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\011\001\012\001\
\013\001\014\001\015\001\255\255\255\255\255\255\255\255\255\255\
\255\255\022\001\023\001\024\001\025\001\026\001\027\001\028\001\
\029\001\003\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\011\001\012\001\013\001\014\001\015\001\255\255\255\255\
\255\255\255\255\255\255\255\255\022\001\023\001\024\001\025\001\
\026\001\027\001\028\001\029\001\003\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\011\001\012\001\013\001\014\001\
\015\001\255\255\255\255\255\255\255\255\255\255\255\255\022\001\
\023\001\024\001\025\001\026\001\027\001\028\001\029\001\003\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\011\001\
\012\001\013\001\014\001\015\001\255\255\255\255\255\255\255\255\
\255\255\255\255\022\001\023\001\024\001\025\001\026\001\027\001\
\028\001\029\001\008\001\255\255\255\255\011\001\012\001\013\001\
\014\001\015\001\255\255\255\255\255\255\255\255\255\255\255\255\
\022\001\023\001\024\001\025\001\026\001\027\001\028\001\029\001\
\011\001\012\001\013\001\014\001\015\001\255\255\255\255\255\255\
\255\255\255\255\255\255\022\001\023\001\024\001\025\001\026\001\
\027\001\028\001\029\001\011\001\012\001\013\001\014\001\015\001\
\255\255\255\255\255\255\255\255\255\255\255\255\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001"

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
  IN\000\
  WITHIN\000\
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
  CLASS\000\
  LIST\000\
  NONE\000\
  RETURN\000\
  DEF\000\
  EOF\000\
  "

let yynames_block = "\
  INT_LITERAL\000\
  FLOAT_LITERAL\000\
  CHAR_LITERAL\000\
  BLIT\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    Obj.repr(
# 36 "parser.mly"
            ( _1)
# 431 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 39 "parser.mly"
                 ( ([], [])               )
# 437 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'decls) in
    Obj.repr(
# 40 "parser.mly"
                    ( ((_1 :: fst _3), snd _3) )
# 445 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'fdecl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decls) in
    Obj.repr(
# 41 "parser.mly"
               ( (fst _2, (_1 :: snd _2)) )
# 453 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    Obj.repr(
# 44 "parser.mly"
              ( [] )
# 459 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl_list) in
    Obj.repr(
# 45 "parser.mly"
                           (  _1 :: _3 )
# 467 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 48 "parser.mly"
         ( (_1, _2) )
# 475 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 51 "parser.mly"
          ( Int   )
# 481 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 52 "parser.mly"
          ( Bool  )
# 487 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 53 "parser.mly"
           ( Float )
# 493 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 54 "parser.mly"
           ( Char )
# 499 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 55 "parser.mly"
           ( Class )
# 505 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 56 "parser.mly"
           ( List )
# 511 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 57 "parser.mly"
          ( None )
# 517 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 60 "parser.mly"
                     ( Int_Literal(_1)        )
# 524 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 61 "parser.mly"
                     ( Float_Literal(_1)      )
# 531 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : char) in
    Obj.repr(
# 62 "parser.mly"
                     ( Char_Literal(_1)       )
# 538 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 63 "parser.mly"
                     ( BoolLit(_1)            )
# 545 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 64 "parser.mly"
                     ( Id(_1)                 )
# 552 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 65 "parser.mly"
                     ( Binop(_1, Add,   _3)   )
# 560 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 66 "parser.mly"
                     ( Binop(_1, Sub,   _3)   )
# 568 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 67 "parser.mly"
                     ( Binop(_1, Times, _3)   )
# 576 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 68 "parser.mly"
                     ( Binop(_1, Divide, _3)  )
# 584 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 69 "parser.mly"
                     ( Binop(_1, Modulo, _3)  )
# 592 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 70 "parser.mly"
                     ( Binop(_1, Equal, _3)   )
# 600 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 71 "parser.mly"
                     ( Binop(_1, Neq, _3)     )
# 608 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 72 "parser.mly"
                     ( Binop(_1, Less,  _3)   )
# 616 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 73 "parser.mly"
                     ( Binop(_1, Greater,_3)  )
# 624 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 74 "parser.mly"
                     ( Binop(_1, And,   _3)   )
# 632 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 75 "parser.mly"
                     ( Binop(_1, Or,    _3)   )
# 640 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 76 "parser.mly"
                     ( Binop(_1, Leq,   _3)   )
# 648 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 77 "parser.mly"
                     ( Binop(_1, Geq,   _3)   )
# 656 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 79 "parser.mly"
                   ( Assign(_1, Binop(Id(_1), Add, Int_Literal(1))) )
# 663 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 80 "parser.mly"
                   ( Assign(_1, Binop(Id(_1), Sub, Int_Literal(1))) )
# 670 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 81 "parser.mly"
                     ( Assign(_1, _3)         )
# 678 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 82 "parser.mly"
                       ( _2                   )
# 685 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 84 "parser.mly"
                              ( Call (_1, _3)  )
# 693 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 87 "parser.mly"
  ( [] )
# 699 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 88 "parser.mly"
                   ( _1 :: _2 )
# 707 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : 'stmt) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'elif_stmt) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 92 "parser.mly"
        ( If ({if_branch= (_3, _5); elif_branch=List.rev _6; else_branch=_8}) )
# 717 "parser.ml"
               : 'if_stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 95 "parser.mly"
    ( [] )
# 723 "parser.ml"
               : 'elif_stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 96 "parser.mly"
                                 ( [(_3 , _5)] )
# 731 "parser.ml"
               : 'elif_stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : 'elif_stmt) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    Obj.repr(
# 97 "parser.mly"
                                                         ((_5 , _7) :: _1 )
# 740 "parser.ml"
               : 'elif_stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 100 "parser.mly"
                                            ( Expr _1      )
# 747 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 101 "parser.mly"
                                            ( Block _2 )
# 754 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'if_stmt) in
    Obj.repr(
# 102 "parser.mly"
                                            ( _1 )
# 761 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 103 "parser.mly"
                                            ( While (_3, _5)  )
# 769 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 104 "parser.mly"
                                                    ( For (_3, _5, _7, _9) )
# 779 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 105 "parser.mly"
                                            ( For_within (_3, _5, _7) )
# 788 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 106 "parser.mly"
                                            ( Return _2      )
# 795 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 107 "parser.mly"
                                            ( Break  )
# 801 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 108 "parser.mly"
                                            ( Continue )
# 807 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 114 "parser.mly"
  (
    {
      rtyp=fst _1;
      fname=snd _1;
      formals=_3;
      locals=_6;
      body=_7
    }
  )
# 825 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 126 "parser.mly"
              ( [] )
# 831 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 127 "parser.mly"
                 ( _1 )
# 838 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 130 "parser.mly"
        ( [_1] )
# 845 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 131 "parser.mly"
                             ( _1::_3 )
# 853 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 135 "parser.mly"
              ( [] )
# 859 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 136 "parser.mly"
         ( _1 )
# 866 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 139 "parser.mly"
        ( [_1] )
# 873 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 140 "parser.mly"
                    ( _1::_3 )
# 881 "parser.ml"
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
