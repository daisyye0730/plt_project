/* Ocamlyacc parser */

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA COLON DOT
%token PLUS MINUS TIMES DIVIDE MODULO PLUSPLUS MINUSMINUS
%token NOT
%token ASSIGN MINUSEQ PLUSEQ
%token EQ NEQ LT LEQ GT GEQ AND OR
%token BREAK CONTINUE IF ELSE ELIF FOR WHILE 
%token INT FLOAT BOOL CHAR LIST NONE STRING
%token RETURN DEF
%token <int> INT_LITERAL
%token <string> FLOAT_LITERAL
%token <char> CHAR_LITERAL
%token <string> STRING_LITERAL
%token <bool> BLIT
%token <string> ID
%token EOF

%start program
%type <Ast.program> program

%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ 
%left PLUS MINUS TIMES DIVIDE MODULO

%%

/* add function declarations*/
program:
  decls EOF { $1}

decls:
   /* nothing */ { ([], [])               }
 | vdecl SEMI decls { (($1 :: fst $3), snd $3) }
 | fdecl decls { (fst $2, ($1 :: snd $2)) }

vdecl_list:
  /*nothing*/ { [] }
  | vdecl SEMI vdecl_list  {  $1 :: $3 }

vdecl:
  typ ID { ($1, $2) }

typ:
    INT   { Int   }
  | BOOL  { Bool  } 
  | FLOAT  { Float } 
  | CHAR   { Char } 
  // | STRUCT { Struct } 
  | LIST LPAREN typ COMMA INT_LITERAL RPAREN  { List($3, $5) } // this will be a type List (int, 4) 
  | NONE  { None }  
  | STRING { String } 

list_index: 
  | ID LBRACKET expr RBRACKET { Access($1, $3) } //myList[0]

expr:
    INT_LITERAL      { Int_Literal($1)        }
  | FLOAT_LITERAL    { Float_Literal($1)      }
  | CHAR_LITERAL     { Char_Literal($1)       }
  | BLIT             { BoolLit($1)            }
  | STRING_LITERAL   { String_Literal($1)     }
  | ID               { Id($1)                 }
  | expr PLUS   expr { Binop($1, Add,   $3)   }
  | expr MINUS  expr { Binop($1, Sub,   $3)   }
  | expr TIMES  expr { Binop($1, Times, $3)   }
  | expr DIVIDE expr { Binop($1, Divide, $3)  }
  | expr MODULO expr { Binop($1, Modulo, $3)  }
  | expr EQ     expr { Binop($1, Equal, $3)   }
  | expr NEQ    expr { Binop($1, Neq, $3)     }
  | expr LT     expr { Binop($1, Less,  $3)   }
  | expr GT     expr { Binop($1, Greater,$3)  }
  | expr AND    expr { Binop($1, And,   $3)   }
  | expr OR     expr { Binop($1, Or,    $3)   }
  | expr LEQ    expr { Binop($1, Leq,   $3)   }
  | expr GEQ    expr { Binop($1, Geq,   $3)   }
  | ID PLUSPLUS    { Assign($1, Binop(Id($1), Add, Int_Literal(1))) }
  | ID MINUSMINUS  { Assign($1, Binop(Id($1), Sub, Int_Literal(1))) }
  | ID ASSIGN expr { Assign($1, $3)         }
  | LPAREN expr RPAREN { $2                   }
  // list support 
  | LBRACKET content_opt RBRACKET {  ListLit($2)      } // [2,3,4]
  // | ID ASSIGN list_index { Assign($1, $3) } // x = mylist[2]
  | list_index ASSIGN expr { ListAssign($1, $3) } // mylist[3] = 5
  | ID LBRACKET INT_LITERAL COLON INT_LITERAL RBRACKET { Slice($1, $3, $5) } //mylist[3:5]
  | ID LBRACKET expr RBRACKET { Access($1, $3)}
  /* call */
  | ID LPAREN args_opt RPAREN { Call ($1, $3)  }

content_opt:
  /*nothing*/ { [] }
  | content_list { $1 }

content_list: 
  expr {[$1]}
  | expr COMMA content_list {$1::$3}

stmt_list: 
  { [] }
  | stmt stmt_list { $1 :: $2 } 

if_stmt: 
  IF LPAREN expr RPAREN stmt elif_stmt ELSE stmt 
        { If ({if_branch= ($3, $5); elif_branch=List.rev $6; else_branch=$8}) }

elif_stmt:
    { [] }
  | ELIF LPAREN expr RPAREN stmt { [($3 , $5)] }
  | elif_stmt LPAREN ELIF LPAREN expr RPAREN stmt RPAREN {($5 , $7) :: $1 }

stmt:
    expr SEMI                               { Expr $1      }
  | LBRACE stmt_list RBRACE                 { Block $2 }
  | if_stmt                                 { $1 }
  | WHILE LPAREN expr RPAREN stmt           { While ($3, $5)  }
  | FOR LPAREN expr SEMI expr SEMI expr RPAREN stmt { For ($3, $5, $7, $9) }
  | RETURN expr SEMI                        { Return $2      }
  | BREAK SEMI                              { Break  }
  | CONTINUE SEMI                           { Continue }


/* fdecl */
fdecl:
  vdecl LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
  {
    {
      rtyp=fst $1;
      fname=snd $1;
      formals=$3;
      locals=$6;
      body=$7
    }
  }

/* formals_opt */
formals_opt:
  /*nothing*/ { [] }
  | formals_list { $1 }

formals_list:
  vdecl { [$1] }
  | vdecl COMMA formals_list { $1::$3 }

/* args_opt*/
args_opt:
  /*nothing*/ { [] }
  | args { $1 }

args:
  expr  { [$1] }
  | expr COMMA args { $1::$3 }
