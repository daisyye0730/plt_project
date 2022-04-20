(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

type sexpr = typ * sx
and sx =
    SInt_Literal of int
  | SFloat_Literal of float
  | SChar_Literal of char
  | SBoolLit of bool
  | SListLit of sexpr list
  | SId of string
  | SBinop of sexpr * op * sexpr
  | SAssign of string * sexpr
  (* call *)
  | SCall of string * sexpr list

type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SIf of 
    {
        sif_branch: sexpr * sstmt;
        selif_branch: (sexpr * sstmt) list;
        selse_branch: sstmt;
    }
  | SWhile of sexpr * sstmt
  | SFor_within of sexpr * sexpr * sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt
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
      | SListLit(l) -> "[" ^ (List.fold_left (fun r e -> r ^ ", " ^ string_of_sexpr e) "" l) ^ " ]"
      | SCall(f, el) ->
          f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
    ) ^ ")"

let rec string_of_sstmt = function
    SBlock(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n"
  | SReturn(expr) -> "return " ^ string_of_sexpr expr ^ ";\n"
  | SIf(if_r) ->   "if (" ^ string_of_sexpr (fst(if_r.sif_branch))  ^ ")\n" ^
      string_of_sstmt (snd(if_r.sif_branch)) ^ 
        List.fold_left (fun str e-> str ^ "\nelif (" ^ string_of_sexpr (fst e) ^ ")\n" ^ string_of_sstmt (snd e)) "" if_r.selif_branch
          ^ "else\n" ^ string_of_sstmt (if_r.selse_branch)
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
