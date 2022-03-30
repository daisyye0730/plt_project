(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Times | Divide | Modulo | Equal | Neq | Less | Greater | And | Or | In | Leq | Geq

type typ = Int | Bool | Float | Char | Class | List | None

type expr =
    Int_Literal of int
  | Float_Literal of float 
  | Char_Literal of char 
  | BoolLit of bool
  | Id of string
  | Binop of expr * op * expr
  | Assign of string * expr
  (* For ID within ID *)
  | Within of string * string
  (* function call *)
  | Call of string * expr list


type stmt =
    Block of stmt list
  | Expr of expr
  (* | If of expr * stmt * stmt *)
  | If of if_record
  | While of expr * stmt
  | For of expr * expr * expr * stmt 
  | For_within of expr * expr * stmt (* For X within Y *)
  | Return of expr
  | Break 
  | Continue
  and if_record = 
    {
        if_branch: expr * stmt;
        elif_branch: (expr * stmt) list;
        else_branch: stmt;
    }

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
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Within(id1, id2) -> "(" ^ id1 ^ " within " ^ id2 ^ ")"

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

let string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | Char -> "char"
  | Class -> "class"
  | List -> "List"
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