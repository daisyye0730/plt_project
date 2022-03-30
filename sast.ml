(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

type sexpr = typ * sx
and sx =
    SInt_Literal of int
  | SFloat_Literal of float
  | SChar_Literal of char
  | SBoolLit of bool
  | SId of string
  | SBinop of sexpr * op * sexpr
  | SAssign of string * sexpr
  (* call *)
  | SCall of string * sexpr list
  | Swithin of string * string
  | SCall of string * sexpr list

type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SWhile of sexpr * sstmt
  | SFor of sexpr * sstmt
  | SFor_within of sexpr * sexpr * sexpr * sstmt
  (* return *)
  | SReturn of sexpr
  | SBreak
  | SContinue

(* func_def: ret_typ fname formals locals body *)
type sfunc_def = {
  srtyp: typ;
  sfname: string;
  sformals: bind list;
  slocals: bind list;
  sbody: sstmt list;
}

type sprogram = bind list * sfunc_def list



(* Pretty-printing functions *)
let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : " ^ (match e with
        SInt_Literal(l) -> string_of_int l
      | SFloat_Literal(l) -> string_of_float l
      | SChar_Literal(l) -> String.make 1 l
      | SBoolLit(true) -> "true"
      | SBoolLit(false) -> "false"
      | SId(s) -> s
      | SBinop(e1, o, e2) ->
        string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
      | SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e
      | SCall(f, el) ->
          f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
    ) ^ ")"
      | SWithin(id1, id2) -> "(" ^ id1 ^ " within " ^ id2 ^ ")"

let rec string_of_sstmt = function
    SBlock(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n"
  | SReturn(expr) -> "return " ^ string_of_sexpr expr ^ ";\n"
  | SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^
                       string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s
  | SBreak -> " break \n"
  | SContinue -> "Continue\n"
  | SFor(e1, e2, e3, s1) -> "For (" ^ string_of_sexpr e1 ^ "; " 
        ^ string_of_sexpr e2 ^ "; " ^ string_of_sexpr e3 ^ ")\n" ^ string_of_sstmt s1 
  | SFor_within(e1, e2, s1) -> "For (" ^ string_of_sexpr e1 ^ "within " 
      ^ string_of_sexpr e2 ^ ")\n" ^ string_of_sstmt s1

let string_of_sfdecl fdecl =
  string_of_typ fdecl.srtyp ^ " " ^
  fdecl.sfname ^ "(" ^ String.concat ", " (List.map snd fdecl.sformals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.slocals) ^
  String.concat "" (List.map string_of_sstmt fdecl.sbody) ^
  "}\n"

let string_of_sprogram (vars, funcs) =
  "\n\nSementically checked program: \n\n" ^
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_sfdecl funcs)
