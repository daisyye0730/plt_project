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
  let i32_t          = L.i32_type    context
  and i8_t           = L.i8_type     context
  and i1_t           = L.i1_type     context 
  and float_t        = L.double_type context 
  and void_t         = L.void_type   context 
  and str_t          = L.pointer_type (L.i8_type context)
  and list_t ty len  = L.array_type ty len  
  in

  (* Return the LLVM type for a dinasour type *)
  let rec ltype_of_typ = function
      A.Int   -> i32_t
    | A.Float -> float_t
    | A.Bool  -> i1_t
    | A.Char -> i8_t
    | A.None -> void_t
    | A.String -> str_t
    | A.List(typ, len) -> list_t (ltype_of_typ typ) len 
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
  let strcmp_t : L.lltype = 
    L.function_type i32_t [| str_t; str_t |] in
  let strcmp_func : L.llvalue = 
    L.declare_function "strcmp" strcmp_t the_module in

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

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder 
    and float_format_str = L.build_global_stringptr "%g\n" "fmt" builder
    and string_format_str = L.build_global_stringptr "%s\n" "fmt" builder
    and char_format_str = L.build_global_stringptr "%c\n" "fmt" builder
    and bool_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    in

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
      and add_local m (t, n) = match t with
        | A.List(ty, len) -> let local_var = 
          L.build_array_alloca (ltype_of_typ ty) (L.const_int i32_t len) n builder 
          in StringMap.add n local_var m
        | _ -> let local_var = L.build_alloca (ltype_of_typ t) n builder 
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
    let rec build_expr builder ((ty, e) : sexpr) = match e with
        SInt_Literal(i)       -> [L.const_int i32_t i]
      | SFloat_Literal(f)     -> [L.const_float_of_string float_t f]
      | SChar_Literal(c)      -> [L.const_int i8_t (Char.code c)]
      | SString_Literal(str)  -> [L.build_global_stringptr str "STRINGLITERAL" builder]
      | SBoolLit b            -> [L.const_int i1_t (if b then 1 else 0)]
      | SId s                 -> [L.build_load (lookup s) s builder]
      | SListLit lp -> List.fold_left (
          fun li e -> let e' = List.hd (build_expr builder e) in e'::li
        ) [] lp
      (* li[2]; *)
      | SAccess(id, idx) -> 
        let idx' = [|L.const_int i32_t idx|] in
        let addr' = L.build_in_bounds_gep (lookup id) idx' "storeLiIndex" builder in
        [L.build_load addr' id builder]
      (* my_list[2] = 5; *)   
      | SListAssign(e1, e2) -> 
        (match e1 with 
         (ty, SAccess(id, idx)) -> let idx' = [|L.const_int i32_t idx|] in
         let addr' = L.build_in_bounds_gep (lookup id) idx' "storeLiIndex" builder in
         let e_eval = List.hd (build_expr builder e2) in 
         ignore(L.build_store e_eval addr' builder); [e_eval]
        | _ -> raise(Failure("semant error in CodeGen: check ListAssign")))
      (* mylist[3: 5]*)
      | SSlice(id, idx_start, idx_end) -> 
        let final = [] in
        let rec match_fun (r, curr_idx) = 
          (if curr_idx = idx_end then r 
          else 
            let addr = 
              L.build_in_bounds_gep (lookup id) [|L.const_int i32_t curr_idx|] "storeLiIndex" builder 
            in 
            let res = L.build_load addr id builder in
            match_fun(res::r, curr_idx+1))
        in match_fun(final, idx_start) 

      | SAssign (s, e) -> 
          (match ty with
            A.List(t, len) ->  
              let index = 0 in 
              let (final, _) = List.fold_left (
                fun r ele -> 
                    let li = (fst r) and idx = (snd r) in 
                    let idx' = [|L.const_int i32_t idx|] in
                    let addr' = L.build_in_bounds_gep (lookup s) idx' "storeLiIndex" builder in
                    ignore(L.build_store ele addr' builder); (ele::li, idx+1))
                ([], index) (List.rev (build_expr builder e))
              in final
          | _ -> let e' = List.hd (build_expr builder e) in
            ignore(L.build_store e' (lookup s) builder); [e'])
      | SBinop (e1, op, e2) ->
        let e1' = List.hd (build_expr builder e1)
        and e2' = List.hd (build_expr builder e2) in
        let t1 = fst e1 and t2 = fst e2 in 
        if t1 = A.Float && t2 = A.Float then 
          [(match op with
            A.Add     -> L.build_fadd
          | A.Sub     -> L.build_fsub
          | A.Times   -> L.build_fmul
          | A.Divide  -> L.build_fdiv
          | A.Modulo  -> L.build_frem
          | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
          | A.Neq     -> L.build_fcmp L.Fcmp.One
          | A.Less    -> L.build_fcmp L.Fcmp.Olt
          | A.Greater -> L.build_fcmp L.Fcmp.Ogt
          | A.Leq     -> L.build_fcmp L.Fcmp.Ole
          | A.Geq     -> L.build_fcmp L.Fcmp.Oge
          | _         -> raise(Failure "semant error in CodeGen: invalid Float Binop")
        ) e1' e2' "tmp" builder]

        else if t1 = A.Int && t2 = A.Int then
        [(match op with
            A.Add     -> L.build_add
          | A.Sub     -> L.build_sub
          | A.Times   -> L.build_mul
          | A.Divide  -> L.build_sdiv
          | A.Modulo  -> L.build_srem
          | A.Equal   -> L.build_icmp L.Icmp.Eq
          | A.Neq     -> L.build_icmp L.Icmp.Ne
          | A.Less    -> L.build_icmp L.Icmp.Slt
          | A.Greater -> L.build_icmp L.Icmp.Sgt
          | A.Leq     -> L.build_icmp L.Icmp.Sle
          | A.Geq     -> L.build_icmp L.Icmp.Sge
          | _         -> raise(Failure "semant error in CodeGen: invalid Int Binop")
        ) e1' e2' "tmp" builder]

        else if t1 = A.Bool && t2 = A.Bool then 
          [(match op with 
            A.And -> L.build_and
          | A.Or  -> L.build_or
          | A.Equal -> L.build_icmp L.Icmp.Eq
          | A.Neq -> L.build_icmp L.Icmp.Ne
          | _ -> raise(Failure "semant error in CodeGen: invalid Bool Binop")
        ) e1' e2' "tmp" builder]

        else if t1 = A.String && t2 = A.String then
        [(match op with 
          | A.Sub     -> L.build_call strcmp_func
          | _ -> raise(Failure "semant error in CodeGen: invalid string Binop")
        ) [| e1';e2' |]  "tmp" builder] 

        (* else if t1 = A.ListLit(ty, len) && t2 = A.ListLit(ty, len) then 
          match (e1, e2) with 
         (ty1, SAccess(id1, )) *  (ty, SAccess(id, idx))-> let idx' = [|L.const_int i32_t idx|] in
        let final = [] in
        let rec match_fun (r, curr_idx) = 
          (if curr_idx = len then r 
          else 
            let addr = 
              L.build_in_bounds_gep (lookup id) [|L.const_int i32_t curr_idx|] "storeLiIndex" builder 
            in 
            let res = L.build_load addr id builder in
            match_fun(res::r, curr_idx+1))
        in match_fun(final, idx_start) 

          [(match ty with 
            A.Bool -> 
          | A.Int -> 
          | A.Float -> 
          | _ -> raise(Failure("CodeGen cannot complete list operation in String datatype. "))
          ) e1' e2' "tmp" builder] *)

        else raise (Failure ("CodeGen match failed in Binop."))
      | SCall ("print", [e]) ->
        [L.build_call printf_func [| int_format_str ; List.hd (build_expr builder e) |]
          "printf" builder]
      | SCall ("strcmp", [e1; e2]) -> 
        [L.build_call strcmp_func  [| List.hd (build_expr builder e1); List.hd (build_expr builder e2) |]
          "strcmp" builder]
      | SCall ("printb", [e]) -> 
        [L.build_call printf_func [| bool_format_str ; List.hd (build_expr builder e) |]
          "printb" builder]
      | SCall ("printf", [e]) -> 
        [L.build_call printf_func [| float_format_str ; List.hd (build_expr builder e) |] 
          "printf" builder]
      | SCall ("printc", [e]) -> 
        [L.build_call printf_func [| char_format_str ; List.hd (build_expr builder e) |] 
          "prints" builder]
      | SCall ("prints", [e]) -> 
        [L.build_call printf_func [| string_format_str ; List.hd (build_expr builder e) |] 
          "prints" builder]
      | SCall (f, args) ->
        let (fdef, fdecl) = StringMap.find f function_decls in
        let llargs = List.rev (List.map (fun e -> List.hd(build_expr builder e)) (List.rev args)) in
        let result = f ^ "_result" in
        [L.build_call fdef (Array.of_list llargs) result builder]
    in 

    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> ignore (instr builder) in

    (* Build the code for the given statement; return the builder for
       the statement's successor (i.e., the next instruction will be built
       after the one generated by this call) *)
    let rec build_stmt (builder, break_bb, continue_bb) = function
        SBlock sl -> List.fold_left build_stmt (builder, break_bb, continue_bb) sl
      | SExpr e -> ignore(List.hd (build_expr builder e)); (builder, break_bb, continue_bb)
      (* unconditional jump to break bb *)
      | SBreak -> ignore(add_terminal builder (L.build_br break_bb)); 
        let builder = L.builder_at_end context break_bb in (builder, break_bb, continue_bb)
      (* unconditional jump to continue bb*)
      | SContinue -> 
        ignore(add_terminal builder(L.build_br continue_bb)); 
        let builder = L.builder_at_end context continue_bb in (builder, break_bb, continue_bb)
      | SReturn e -> ignore(L.build_ret (List.hd (build_expr builder e)) builder); (builder, break_bb, continue_bb)    
      | SIf (if_r) -> 
          (* construct if then-stmt *) 
          let bool_val_1 = List.hd (build_expr builder (fst(if_r.sif_branch))) in
          let l1_body_bb = L.append_block context "L1_body" the_function in
          let curr_builder = L.builder_at_end context l1_body_bb in
          let then_stmt = snd(if_r.sif_branch) in
          let (b, _, _) = build_stmt(curr_builder, break_bb, continue_bb) then_stmt in
          ignore(b);

          let count = 2 in
          let (_, elif_li) = List.fold_left (fun res e -> 
            let r = snd res in 
            let count = fst res in 
            let bool_val = List.hd (build_expr builder (fst e)) in 
            let bb = (L.append_block context ("L"^string_of_int count) the_function) in
            let body_bb = (L.append_block context ("L"^string_of_int count^"_body") the_function) in
            let curr_builder = L.builder_at_end context body_bb in 
            let then_stmt = snd e in 
            let (b, _, _) = build_stmt(curr_builder, break_bb, continue_bb) then_stmt in
            ignore(b);
            (count+1, (bool_val, L.builder_at_end context bb, bb, body_bb)::r);
          ) (count, [(bool_val_1, builder, break_bb, l1_body_bb)]) if_r.selif_branch in
          (* counstruct last else bb*)
          let count = count + 1 in
          let else_bb = L.append_block context ("L"^string_of_int count^"_body") the_function in
          let curr_builder = L.builder_at_end context else_bb in 
          let then_stmt = if_r.selse_branch in 
          let (b, _, _) = build_stmt(curr_builder, break_bb, continue_bb) then_stmt in
          ignore(b);
          let end_bb = L.append_block context "L_end" the_function in
          (* elif_li looks like 
            [(bool_val_3, L3 builder, L3 bb, L3_body),
            (bool_val_2, L2, L2 bb, L2_body),
            (bool_val_1, builder, break_bb, L1_body)] *)
          (* elif_li_complete looks like 
            [(bool_val_1, builder, break_bb, L1_body, L2), 
            (bool_val_2, L2 builder, L2, L2_body, L3), 
            (bool_val_3, L3 builder, L3, L3_body, L4), 
            (None, Ln_body, else_bb, else_bb, L_end)] *)
          let (elif_li_complete, _) = List.fold_left (fun r e -> 
            let li = fst r in 
            let next = snd r in
            let (bool_val, bb_builder, bb, body_bb) = e in 
            ((bool_val, bb_builder, bb, body_bb, next)::li, bb)
          ) ([(bool_val_1, L.builder_at_end context else_bb, else_bb, else_bb, end_bb)], else_bb) elif_li in
          (* add bz jumps *)
          List.fold_left (fun r e -> 
            let (bool_val, builder, builder_bb, builder_body_bb, next_bb) = e in 
            ignore(L.build_cond_br bool_val builder_body_bb next_bb builder);
            e::r
          ) [] elif_li_complete;
  
          (* add jmp L_end *)
          let build_br_end = L.build_br end_bb in (* partial function *)
          List.fold_left (fun r e -> 
            let (bool_val, builder, builder_bb, builder_body_bb, next_bb) = e in
            ignore(add_terminal (L.builder_at_end context builder_body_bb) build_br_end);
            e::r
          )
          [] elif_li_complete;
          (L.builder_at_end context end_bb, break_bb, continue_bb);
      | SWhile (predicate, body) ->
        let while_predicate_bb = L.append_block context "while_predicate_block" the_function in
        ignore (L.build_br while_predicate_bb builder);
        let while_body_bb = L.append_block context "while_body_block" the_function in
        let while_merge_bb = L.append_block context "while_merge_block" the_function in
        let break_builder = while_merge_bb and continue_builder = while_predicate_bb in
        let b = L.builder_at_end context while_body_bb in
        let (temp, _, _) = build_stmt (b, break_builder, continue_builder) body in
        ignore(add_terminal temp (L.build_br while_predicate_bb)); 
        let predicate_builder = L.builder_at_end context while_predicate_bb in
        let bool_val = List.hd (build_expr predicate_builder predicate) in
        ignore (L.build_cond_br bool_val while_body_bb while_merge_bb predicate_builder);
        ((L.builder_at_end context while_merge_bb), break_builder, continue_builder)
      | SFor(e1, e2, e3, s1) -> build_stmt(builder, break_bb, continue_bb) 
        (SBlock[SExpr e1; SWhile (e2, SBlock [s1; SExpr e3])])
    
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




    (* LLVM insists each basic block end with exactly one "terminator"
       instruction that transfers control.  This function runs "instr builder"
       if the current block does not already have a terminator.  Used,
       e.g., to handle the "fall off the end of the function" case. *)
    (* let add_terminal builder instr =
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
        let builder = L.builder_at_end context break_bb in (builder, break_bb, continue_bb)
      (* unconditional jump to continue bb*)
      | SContinue -> 
        ignore(add_terminal builder(L.build_br continue_bb)); 
        let builder = L.builder_at_end context continue_bb in (builder, break_bb, continue_bb)
      | SReturn e -> ignore(L.build_ret (build_expr builder e) builder); (builder, break_bb, continue_bb)    
      | SIf (if_r) -> 
          (* construct if then-stmt *) 
          let bool_val_1 = build_expr builder (fst(if_r.sif_branch)) in
          let l1_body_bb = L.append_block context "L1_body" the_function in
          let curr_builder = L.builder_at_end context l1_body_bb in
          let then_stmt = snd(if_r.sif_branch) in
          let (b, _, _) = build_stmt(curr_builder, break_bb, continue_bb) then_stmt in
          ignore(b);

          let count = 2 in
          let (_, elif_li) = List.fold_left (fun res e -> 
            let r = snd res in 
            let count = fst res in 
            let bool_val = build_expr builder (fst e) in 
            let bb = (L.append_block context ("L"^string_of_int count) the_function) in
            let body_bb = (L.append_block context ("L"^string_of_int count^"_body") the_function) in
            let curr_builder = L.builder_at_end context body_bb in 
            let then_stmt = snd e in 
            let (b, _, _) = build_stmt(curr_builder, break_bb, continue_bb) then_stmt in
            ignore(b);
            (count+1, (bool_val, L.builder_at_end context bb, bb, body_bb)::r);
          ) (count, [(bool_val_1, builder, break_bb, l1_body_bb)]) if_r.selif_branch in
          (* counstruct last else bb*)
          let count = count + 1 in
          let else_bb = L.append_block context ("L"^string_of_int count^"_body") the_function in
          let curr_builder = L.builder_at_end context else_bb in 
          let then_stmt = if_r.selse_branch in 
          let (b, _, _) = build_stmt(curr_builder, break_bb, continue_bb) then_stmt in
          ignore(b);
          let end_bb = L.append_block context "L_end" the_function in
          (* elif_li looks like 
            [(bool_val_3, L3 builder, L3 bb, L3_body),
            (bool_val_2, L2, L2 bb, L2_body),
            (bool_val_1, builder, break_bb, L1_body)] *)
          (* elif_li_complete looks like 
            [(bool_val_1, builder, break_bb, L1_body, L2), 
            (bool_val_2, L2 builder, L2, L2_body, L3), 
            (bool_val_3, L3 builder, L3, L3_body, L4), 
            (None, Ln_body, else_bb, else_bb, L_end)] *)
          let (elif_li_complete, _) = List.fold_left (fun r e -> 
            let li = fst r in 
            let next = snd r in
            let (bool_val, bb_builder, bb, body_bb) = e in 
            ((bool_val, bb_builder, bb, body_bb, next)::li, bb)
          ) ([(bool_val_1, L.builder_at_end context else_bb, else_bb, else_bb, end_bb)], else_bb) elif_li in
          (* add bz jumps *)
          List.fold_left (fun r e -> 
            let (bool_val, builder, builder_bb, builder_body_bb, next_bb) = e in 
            ignore(L.build_cond_br bool_val builder_body_bb next_bb builder);
            e::r
          ) [] elif_li_complete;
  
          (* add jmp L_end *)
          let build_br_end = L.build_br end_bb in (* partial function *)
          List.fold_left (fun r e -> 
            let (bool_val, builder, builder_bb, builder_body_bb, next_bb) = e in
            ignore(add_terminal (L.builder_at_end context builder_body_bb) build_br_end);
            e::r
          )
          [] elif_li_complete;
          (L.builder_at_end context end_bb, break_bb, continue_bb);
      | SWhile (predicate, body) ->
        let while_predicate_bb = L.append_block context "while_predicate_block" the_function in
        ignore (L.build_br while_predicate_bb builder);
        let while_body_bb = L.append_block context "while_body_block" the_function in
        let while_merge_bb = L.append_block context "while_merge_block" the_function in
        let break_builder = while_merge_bb and continue_builder = while_predicate_bb in
        let b = L.builder_at_end context while_body_bb in
        let (temp, _, _) = build_stmt (b, break_builder, continue_builder) body in
        ignore(add_terminal temp (L.build_br while_predicate_bb)); 
        let predicate_builder = L.builder_at_end context while_predicate_bb in
        let bool_val = build_expr predicate_builder predicate in
        ignore (L.build_cond_br bool_val while_body_bb while_merge_bb predicate_builder);
        ((L.builder_at_end context while_merge_bb), break_builder, continue_builder)
      | SFor(e1, e2, e3, s1) -> build_stmt(builder, break_bb, continue_bb) 
        (SBlock[SExpr e1; SWhile (e2, SBlock [s1; SExpr e3])])
    
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
  the_module *)
