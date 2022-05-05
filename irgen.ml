(* IR generation: translate takes a semantically checked AST and
   produces LLVM IR

   LLVM tutorial: Make sure to read the OCaml version of the tutorial

   http://llvm.org/docs/tutorial/index.html

   Detailed documentation on the OCaml LLVM library:

   http://llvm.moe/
   http://llvm.moe/ocaml/

*)

module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate (globals, functions) =
  let context    = L.global_context () in

  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "dinosaur" in

  (* Get types from the context *)
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context 
  and float_t    = L.double_type context 
  and void_t     = L.void_type   context 
  and str_t      = L.pointer_type (L.i8_type context) 
  in

  (* Return the LLVM type for a dinasour type *)
  let ltype_of_typ = function
      A.Int   -> i32_t
    | A.Float -> float_t
    | A.Bool  -> i1_t
    | A.Char -> i1_t
    | A.None -> void_t
    | A.String -> str_t
    (* | A.List(typ, len) ->  *)
  in

  (* Create a map of global variables after creating each *)
  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n) =
      let init = match t with
        A.Int -> L.const_int (ltype_of_typ t) 0
      | A.Float -> L.const_float (ltype_of_typ t) 0.0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in

  let printf_t : L.lltype =
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue =
    L.declare_function "printf" printf_t the_module in

  (* Define each function (arguments and return type) so we can
     call it even before we've created its body *)
  let function_decls : (L.llvalue * sfunc_def) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types =
        Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
      in let ftype = L.function_type (ltype_of_typ fdecl.srtyp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in

  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n) p =
        L.set_value_name n p;
        let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore (L.build_store p local builder);
        StringMap.add n local m

      (* Allocate space for any locally declared variables and add the
       * resulting registers to our map *)
      and add_local m (t, n) =
        let local_var = L.build_alloca (ltype_of_typ t) n builder
        in StringMap.add n local_var m
      in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.sformals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.slocals
    in

    (* Return the value for a variable or formal argument.
       Check local names first, then global names *)
    let lookup n = try StringMap.find n local_vars
      with Not_found -> StringMap.find n global_vars
    in

    (* Construct code for an expression; return its value *)
    let rec build_expr builder ((_, e) : sexpr) = match e with
        SInt_Literal(i)  -> L.const_int i32_t i
      | SFloat_Literal(f) -> L.const_float_of_string float_t f
      | SChar_Literal(c) -> L.const_int i8_t (Char.code c)
      | SString_Literal(str)-> L.build_global_stringptr str "STRINGLITERAL" builder
      | SBoolLit b  -> L.const_int i1_t (if b then 1 else 0)
      | SId s       -> L.build_load (lookup s) s builder
      | SAssign (s, e) -> let e' = build_expr builder e in
        ignore(L.build_store e' (lookup s) builder); e'
      | SBinop (e1, op, e2) ->
        let e1' = build_expr builder e1
        and e2' = build_expr builder e2 in
        (match op with
           A.Add     -> L.build_add
         | A.Sub     -> L.build_sub
         | A.Times   -> L.build_mul
         | A.Divide  -> L.build_sdiv
         | A.Modulo  -> L.build_srem
         | A.And     -> L.build_and
         | A.Or      -> L.build_or
         | A.Equal   -> L.build_icmp L.Icmp.Eq
         | A.Neq     -> L.build_icmp L.Icmp.Ne
         | A.Less    -> L.build_icmp L.Icmp.Slt
         | A.Greater -> L.build_icmp L.Icmp.Sgt
         | A.Leq     -> L.build_icmp L.Icmp.Sle
         | A.Geq     -> L.build_icmp L.Icmp.Sge
        ) e1' e2' "tmp" builder
      | SCall ("print", [e]) ->
        L.build_call printf_func [| int_format_str ; (build_expr builder e) |]
          "printf" builder
      | SCall (f, args) ->
        let (fdef, fdecl) = StringMap.find f function_decls in
        let llargs = List.rev (List.map (build_expr builder) (List.rev args)) in
        let result = f ^ "_result" in
        L.build_call fdef (Array.of_list llargs) result builder
    in

    (* LLVM insists each basic block end with exactly one "terminator"
       instruction that transfers control.  This function runs "instr builder"
       if the current block does not already have a terminator.  Used,
       e.g., to handle the "fall off the end of the function" case. *)
    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> ignore (instr builder) in

    (* Build the code for the given statement; return the builder for
       the statement's successor (i.e., the next instruction will be built
       after the one generated by this call) *)
    let rec build_stmt (builder, break_bb, continue_bb) = function
        SBlock sl -> List.fold_left build_stmt (builder, break_bb, continue_bb) sl
      | SExpr e -> ignore(build_expr builder e); (builder, break_bb, continue_bb)
      (* unconditional jump to break bb *)
      | SBreak -> ignore(add_terminal builder (L.build_br break_bb)); 
        let next_bb = L.append_block context "after_break" the_function in 
        let builder = L.builder_at_end context next_bb in (builder, break_bb, continue_bb)
      (* unconditional jump to continue bb*)
      | SContinue -> ignore(add_terminal builder(L.build_br continue_bb));
        let continue_bb = L.append_block context "after_continue" the_function in 
        let builder = L.builder_at_end context continue_bb in (builder, break_bb, continue_bb)
      | SReturn e -> ignore(L.build_ret (build_expr builder e) builder); (builder, break_bb, continue_bb)
      (* if(E1) {S1} elif(E2) {S2} else{S3} *)
        (* three address code 
        L1: E1.code 
        bz L2 E1.addr
        -------------
        L1_body: S1.code 
        jmp L_end
        -------------
        L2: E2.code 
        bz L3 E2.addr 
        -------------
        L2_body: S2.code 
        jmp L_end
        ------------
        L3: L3.code 
        jmp L_end 
        ------------
        L_end: *)
      | SIf (if_r) ->  
        let bool_val_1 = build_expr builder (fst(if_r.sif_branch)) in
        let l1_body_bb = L.append_block context "L1_body" the_function in
        let curr_builder = L.builder_at_end context l1_body_bb in
        let then_stmt = snd(if_r.sif_branch) in
        let (b, _, _) = build_stmt(curr_builder, break_bb, continue_bb) then_stmt in
        ignore(b);
        
        let bool_val_2 = build_expr builder (fst(List.hd if_r.selif_branch)) in
        let l2_bb = L.append_block context "L2" the_function in
        let l2_body_bb = L.append_block context "L2_body" the_function in
        let curr_builder = L.builder_at_end context l2_body_bb in
        let then_stmt = snd(List.hd if_r.selif_branch) in
        let (b, _, _) = build_stmt(curr_builder, break_bb, continue_bb) then_stmt in
        ignore(b);

        let l3_bb = L.append_block context "L3" the_function in
        let curr_builder = L.builder_at_end context l3_bb in 
        let then_stmt = if_r.selse_branch in 
        let (b, _, _) = build_stmt(curr_builder, break_bb, continue_bb) then_stmt in
        ignore(b);

        (* add bz jumps *)
        ignore(L.build_cond_br bool_val_1 l1_body_bb l2_bb builder);
        ignore(L.build_cond_br bool_val_2 l2_body_bb l3_bb (L.builder_at_end context l2_bb));

        (* add jmp L_end *)
        let end_bb = L.append_block context "L_end" the_function in
        let build_br_end = L.build_br end_bb in (* partial function *)
        add_terminal (L.builder_at_end context l1_body_bb) build_br_end;
        add_terminal (L.builder_at_end context l2_body_bb) build_br_end; 
        add_terminal (L.builder_at_end context l3_bb) build_br_end;

        (L.builder_at_end context end_bb, break_bb, continue_bb);
        
      (* | SWhile (predicate, body) ->
        let while_bb = L.append_block context "while" the_function in
        let build_br_while = L.build_br while_bb in (* partial function *)
        ignore (build_br_while builder);
        let while_builder = L.builder_at_end context while_bb in
        let bool_val = build_expr while_builder predicate in

        let body_bb = L.append_block context "while_body" the_function in
        add_terminal (build_stmt (L.builder_at_end context body_bb) body) build_br_while;

        let end_bb = L.append_block context "while_end" the_function in

        ignore(L.build_cond_br bool_val body_bb end_bb while_builder);
        L.builder_at_end context end_bb *)

    in
    (* Build the code for each statement in the function *)
    let bb = L.append_block context "dummy_block" the_function in
    let break_builder = bb and continue_builder = bb in 
    let (func_builder, _, _) = (build_stmt (builder, break_builder, continue_builder) (SBlock fdecl.sbody)) in

    (* Add a return if the last block falls off the end *)
    add_terminal func_builder (L.build_ret (L.const_int i32_t 0));
    ignore(L.builder_at_end context bb);
    ignore(L.block_terminator bb);
    ignore(L.delete_block bb);

  in

  List.iter build_function_body functions;
  the_module
