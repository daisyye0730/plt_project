/* Ocamlyacc parser */

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA COLON DOT
%token PLUS MINUS TIMES DIVIDE MODULO
%token NOT
%token ASSIGN
%token EQ NEQ LT LEQ GT GEQ AND OR IN
%token WITHIN BREAK CONTINUE IF ELSE ELIF FOR WHILE 
%token INT FLOAT BOOL CHAR CLASS LIST NONE
%token RETURN DEF
%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL
%token <char> CHAR_LITERAL
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
  vdecl_list stmt_list EOF { {locals=$1; body=$2} }

vdecl_list:
  { [] }
  | vdecl vdecl_list { $1 :: $2 }

vdecl:
  typ ID SEMI { ($1, $2) }

decls:
   /* nothing */ { ([], [])               }
 | vdecl SEMI decls { (($1 :: fst $3), snd $3) }
 | fdecl decls { (fst $2, ($1 :: snd $2)) }

/* int x */
vdecl:
  typ ID { ($1, $2) }

expr:
    LITERAL          { Literal($1)            }
  | BLIT             { BoolLit($1)            }
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
  | expr IN     expr { Binop($1, In,    $3)   } 
  | ID   WITHIN ID   { Within($1, $3)         }
  | ID   ASSIGN expr { Assign($1, $3)         }
  | LPAREN expr RPAREN { $2                   }
  /* call */
  | ID LPAREN args_opt RPAREN { Call ($1, $3)  }

typ:
    INT   { Int   }
  | BOOL  { Bool  } 
  | FLOAT  { Float } 
  | CHAR   { Char } 
  // | "True"   { BLIT(true)  }
  // | "False"  { BLIT(false) }
  | CLASS  { Class } 
  | LIST   { List }
  | NONE  { None }

stmt_list: 
  { [] }
  |stmt stmt_list { $1 :: $2 } 

elif_stmt:
  stmt { $1 }
  | stmt ELIF LPAREN expr RPAREN elif_stmt { Elif($1, $4, $6)}

stmt:
    expr SEMI                               { Expr $1      }
  | LBRACE stmt_list RBRACE                 { Block $2 }
  /* if (condition) { block1 } else { block2 } */
  | IF LPAREN expr RPAREN elif_stmt ELSE stmt    { If($3, $5, $7) }
  | WHILE LPAREN expr RPAREN stmt           { While ($3, $5)  }
  | FOR LPAREN expr SEMI expr SEMI expr RPAREN stmt { For ($3, $5, $7, $9) }
  /*for (number within mylist) {
		number = number + 1;
	}
  */
  | FOR LPAREN expr WITHIN expr RPAREN stmt { For_within ($3, $5, $6) }
  /* return */
  | RETURN expr SEMI                        { Return $2      }


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
