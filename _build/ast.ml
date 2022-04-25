(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Times | Divide | Modulo | Equal | Neq | Less | Greater | And | Or | In | Leq | Geq

type typ = Int | Bool | Float | Char | Class | List of typ * int | None

type expr =
    Int_Literal of int
  | Float_Literal of float 
  | Char_Literal of char 
  | BoolLit of bool
  | ListLit of expr list
  | Id of string
  | Binop of expr * op * expr
  | Assign of string * expr
  | ListAssign of expr * expr 
  | Access of string * int 
  | Slice of string * int * int
  (* function call *)
  | Call of string * expr list


type stmt =
    Block of stmt list
  | Expr of expr
  (* | If of expr * stmt * stmt *)
  | If of {
    if_branch: expr * stmt;
    elif_branch: (expr * stmt) list;
    else_branch: stmt;
  }
  | While of expr * stmt
  | For of expr * expr * expr * stmt 
  | For_within of expr * expr * stmt (* For X within Y *)
  | Return of expr
  | Break 
  | Continue

(* int x: name binding *)
type bind = typ * string

(* func_def: ret_typ fname formals locals body *)
type func_def = {
  rtyp: typ;
  fname: string;
  formals: bind list;
  locals: bind list;
  body: stmt list;
}

type program = bind list * func_def list

(* Pretty-printing functions *)
let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Times -> "*"
  | Divide -> "/"
  | Modulo -> "%"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Greater -> ">"
  | And -> "&&"
  | Or -> "||"
  | In -> "in"
  | Leq -> "<="
  | Geq -> ">="

let rec string_of_expr = function
    Int_Literal(l) -> string_of_int l
  | Float_Literal(l) -> string_of_float l
  | Char_Literal(l) -> String.make 1 l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | ListAssign (expr, e) -> string_of_expr expr ^ "=" ^ string_of_expr e
  | Access(id, idx) -> id ^ "[" ^ string_of_int idx ^ "]"
  | ListLit(l) -> "[" ^ (List.fold_left (fun r e -> r ^ ", " ^ string_of_expr e) "" l) ^ " ]"
  | Slice(id, index1, index2) -> id ^ "[" ^ string_of_int index1 ^ ":" ^ string_of_int index2 ^ "]"
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"


let rec string_of_stmt = function
    Block(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n"
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n"
  | If(if_r) ->   "if (" ^ string_of_expr (fst(if_r.if_branch))  ^ ")\n" ^
                      string_of_stmt (snd(if_r.if_branch)) ^ 
                  List.fold_left (fun str e-> str ^ "\nelif (" ^ string_of_expr (fst e) ^ ")\n" ^ string_of_stmt (snd e)) "" if_r.elif_branch
                  ^ "else\n" ^ string_of_stmt (if_r.else_branch)
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | Break -> " break \n"
  | Continue -> "Continue\n"
  | For(e1, e2, e3, s1) -> "For (" ^ string_of_expr e1 ^ "; " 
        ^ string_of_expr e2 ^ "; " ^ string_of_expr e3 ^ ")\n" ^ string_of_stmt s1 
  | For_within(e1, e2, s1) -> "For (" ^ string_of_expr e1 ^ "within " 
      ^ string_of_expr e2 ^ ")\n" ^ string_of_stmt s1

let rec string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | Char -> "char"
  | Class -> "class"
  | List (typ, integer) -> "List(" ^ string_of_typ typ ^ ", " ^ string_of_int integer ^ ")"
  | None -> "None"

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.rtyp ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  "\n\nParsed program: \n\n" ^
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)