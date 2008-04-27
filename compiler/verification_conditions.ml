(* PiVC *)

open Basic_paths
open Expr_utils
open Ast

exception InvalidPath of string ;;

(* Converts all length() nodes to specially named idents.
   Adds a length assignment step to complement each array assignment step.
*)
let convert_basic_path_to_acceptable_form path = 
  let get_new_length_identifier expr = 
    match expr with 
        Length(loc,exp) -> 
          begin
            let ident = identifier_of_array_expr exp in
            let new_ident = Ast.create_length_identifier ("|" ^ ident.name ^ "|") (gdl()) in
            let vd = Ast.create_varDecl (Int(gdl())) new_ident (varDecl_of_identifier ident).location_vd in
              vd.var_id := Some("_length_"^id_of_identifier ident);
              new_ident.decl := Some(vd);
              new_ident
          end
      | _ -> assert(false)
  in
  let get_new_length_expr expr = 
    Ast.LValue(Ast.gdl(),Ast.NormLval(Ast.gdl(),get_new_length_identifier expr))
  in
  let replace_length_with_var expr = 
    let rec rlwv expr = match expr with
        Assign(loc,f,t) -> Assign(loc,f,rlwv t)
      | Constant (loc,c) -> expr
      | LValue (loc,l) -> expr
      | Call (loc,s, el) ->
          begin
            let new_args = List.map rlwv el in
              Call(loc,s,new_args)
          end
      | Plus (loc,t1, t2) -> Plus(loc,rlwv t1, rlwv t2)
      | Minus (loc,t1, t2) -> Minus(loc,rlwv t1, rlwv t2)
      | Times (loc,t1, t2) -> Times(loc,rlwv t1, rlwv t2)
      | Div (loc,t1, t2) -> Div(loc,rlwv t1, rlwv t2)
      | IDiv (loc,t1, t2) -> IDiv(loc,rlwv t1, rlwv t2)
      | Mod (loc,t1, t2) -> Mod(loc,rlwv t1, rlwv t2)
      | UMinus (loc,t) -> UMinus(loc,rlwv t)
      | ForAll (loc,decls,e) -> ForAll(loc,decls,rlwv e)
      | Exists (loc,decls,e) -> Exists(loc,decls,rlwv e)
      | ArrayUpdate (loc,exp,assign_to,assign_val) -> ArrayUpdate(loc,rlwv exp, rlwv assign_to, rlwv assign_val)
      | LT (loc,t1, t2) -> LT(loc,rlwv t1, rlwv t2)
      | LE (loc,t1, t2) -> LE(loc,rlwv t1, rlwv t2)
      | GT (loc,t1, t2) -> GT(loc,rlwv t1, rlwv t2)
      | GE (loc,t1, t2) -> GE(loc,rlwv t1, rlwv t2)
      | EQ (loc,t1, t2) -> EQ(loc,rlwv t1, rlwv t2)
      | NE (loc,t1, t2) -> NE(loc,rlwv t1, rlwv t2)
      | And (loc,t1, t2) -> And(loc,rlwv t1, rlwv t2)
      | Or (loc,t1, t2) -> Or(loc,rlwv t1, rlwv t2)
      | Not (loc,t) -> Not(loc,rlwv t)
      | Iff (loc,t1, t2) -> Iff(loc,rlwv t1, rlwv t2)
      | Implies (loc,t1, t2) -> Implies(loc,rlwv t1, rlwv t2)
      | Length (loc, t) -> get_new_length_expr expr
      | EmptyExpr  -> expr
    in
      rlwv expr
  in
  let replace_length_with_var_in_step step = 
    match step with
        Basic_paths.Expr(e) -> Basic_paths.Expr(replace_length_with_var e)
      | Assume(e) -> Assume(replace_length_with_var e)
      | Annotation(e,s) -> Annotation(replace_length_with_var e, s)
      | RankingAnnotation (ra) -> RankingAnnotation({ tuple = List.map (function e -> replace_length_with_var e) ra.tuple; location_ra = ra.location_ra })
  in
  let add_extra_node_for_length_if_necessary node =
      begin
        match node with
            Basic_paths.Expr (e) ->
              begin
                match e with
                    Ast.Assign(loc,lval,expr) -> 
                      begin
                        match lval with
                            Ast.NormLval(loc,id) ->
                              begin
                                match (Ast.varDecl_of_identifier id).Ast.varType with
                                    Ast.Array(t,loc) ->
                                      begin
                                        let lhs = Ast.NormLval(gdl(), get_new_length_identifier (Length(Ast.gdl(),Ast.LValue(Ast.gdl(),Ast.NormLval(Ast.gdl(),id))))) in
                                        let rhs = get_new_length_expr (Length(Ast.gdl(),Ast.LValue(Ast.gdl(),Ast.NormLval(Ast.gdl(),Ast.identifier_of_array_expr expr)))) in
                                          [Basic_paths.Expr(Ast.Assign(Ast.gdl(),lhs,rhs));replace_length_with_var_in_step node]
                                      end
                                  | _ -> [replace_length_with_var_in_step node]
                              end
                          | _ -> [replace_length_with_var_in_step node]
                      end
                  | _ -> [replace_length_with_var_in_step node]
              end
          | _ -> [replace_length_with_var_in_step node]
      end
  in
  let rec glo path = 
    match path with
        node :: nodes -> (add_extra_node_for_length_if_necessary node) @ (glo nodes)
      | [] -> []
  in
    glo path
        
let add_array_length_greater_than_0_to_expr expr = 
  let rec gl exp =
    match exp with
    | Assign (loc,l, e) -> assert(false)
    | Constant (loc,c) -> []
    | LValue (loc,l) ->
        begin
          match l with
              NormLval(loc,ident) -> if ident.is_length then [exp] else []
            | ArrayLval(loc,exp1,exp2) -> []
        end
    | Call (loc,s, el) -> assert(false)
    | Plus (loc,t1, t2) -> gl t1 @ gl t2
    | Minus (loc,t1, t2) -> gl t1 @ gl t2
    | Times (loc,t1, t2) -> gl t1 @ gl t2
    | Div (loc,t1, t2) -> gl t1 @ gl t2
    | IDiv (loc,t1, t2) -> gl t1 @ gl t2
    | Mod (loc,t1, t2) -> gl t1 @ gl t2
    | UMinus (loc,t) -> gl t
    | ForAll (loc,decls,e) -> gl e
    | Exists (loc,decls,e) -> gl e
    | ArrayUpdate (loc, exp, assign_to, assign_val) -> []
    | LT (loc,t1, t2) -> gl t1 @ gl t2
    | LE (loc,t1, t2) -> gl t1 @ gl t2
    | GT (loc,t1, t2) -> gl t1 @ gl t2
    | GE (loc,t1, t2) -> gl t1 @ gl t2
    | EQ (loc,t1, t2) -> gl t1 @ gl t2
    | NE (loc,t1, t2) -> gl t1 @ gl t2
    | And (loc,t1, t2) -> gl t1 @ gl t2
    | Or (loc,t1, t2) -> gl t1 @ gl t2
    | Not (loc,t) -> gl t
    | Length (loc, t) -> assert(false)
    | Iff (loc,t1, t2) -> gl t1 @ gl t2
    | Implies (loc,t1, t2) -> gl t1 @ gl t2
    | EmptyExpr -> []
  in
  let geq_0_expr_of_expr expr = Not(gdl(),LT(gdl(),expr,Constant(gdl(),ConstInt(gdl(),0)))) in
  let all_geq_0_exprs = conjuncts_of_exprs (List.map geq_0_expr_of_expr (Expr_utils.remove_duplicates_from_list (gl expr))) in
    match all_geq_0_exprs with
        EmptyExpr -> expr
      | _ -> Implies(gdl(),all_geq_0_exprs,expr)
      

(* Gets a VC out of a basic path.
   Returns the VC as an Ast.Expr. *)
let get_vc bp =
  let path_with_length_nodes = get_steps_from_path bp in
  let is_termination_path = Basic_paths.is_termination_path bp in
  let path = convert_basic_path_to_acceptable_form path_with_length_nodes in
  let dummy_loc = Ast.get_dummy_location () in

  (* Weakest precondition on a list in reverse order: wp(formula, in; ...; i2; i1) *)
  let rec wp formula rev_instrs =

    (* Weakest precondition on a single instruction: wp(formula, instr) *)
    let single_wp formula instr =
      match instr with
      | Assume (e) ->
	  Ast.Implies (dummy_loc, e, formula)
      | Basic_paths.Expr (exp) ->
	  begin
	    match exp with
	      | Ast.Assign (_,l,e) ->
                  begin
                    match l with
                        Ast.NormLval(loc,id) -> sub_idents_in_expr formula [(id, e)]
                      | Ast.ArrayLval(loc,arr,index) -> (*sub_idents_in_expr formula [((array_ident_from_lval l),e)]*)
                          begin
                            let array_ident = array_ident_from_lval l in
                            let array_update = array_write_to_array_update (Ast.LValue(Ast.get_dummy_location(),l)) e in
                              match array_update with
                                  Ast.Assign(loc,update_from,update_to) -> sub_idents_in_expr formula [(array_ident,update_to)]
                                | _ -> assert(false)
                          end
                  end
	      | _ -> formula
	  end
      | _ -> raise (InvalidPath "Annotation in middle of path")
    in
    
    if List.length rev_instrs = 0 then
      (* For basic paths without any instructions, the VC is precondition -> postcondition. *)
      formula 
    else if List.length rev_instrs = 1 then
      single_wp formula (List.hd rev_instrs)
    else
      wp (single_wp formula (List.hd rev_instrs)) (List.tl rev_instrs)
  in

  (* Make the VC itself. *)
  let vc_to_return =
    (* Gets the Ast.Expr out of a path node. *)
    let get_expr_from_path_node n = match n with
      | Annotation (e, _) -> e
      | _ -> raise (InvalidPath "No Annotation where expected in path")
    in
    let start_ann = get_expr_from_path_node (List.hd path) in
    (* Handle normal and runtime assertion basic paths. *)
    if (not is_termination_path) then
      let rev_list = List.rev (List.tl path) in
      let rev_instrs = List.tl rev_list in
      let end_ann = get_expr_from_path_node (List.hd rev_list) in
      Ast.Implies (dummy_loc, start_ann, wp end_ann rev_instrs)
    else
      (* Handle termination basic paths. *)
      begin
	let get_tuple n = match n with
	  | RankingAnnotation (ra) -> ra.tuple
	  | _ -> raise (InvalidPath "No RankingAnnotation where expected in path")
	in
	let start_tuple = get_tuple (List.hd (List.tl path)) in
	let rev_list = List.rev (List.tl (List.tl path)) in
	let end_tuple = get_tuple (List.hd rev_list) in
	(* Get the Ast representation of < in the lexographic ordering of the two tuples. *)
	let ordering_expr =
	  let single_ordering (prev_lt_opt, prev_and_opt) cur_start cur_end =
	    let cur_lt = Ast.LT (dummy_loc, cur_end, cur_start) in
	    let cur_eq = Ast.EQ (dummy_loc, cur_end, cur_start) in
	    if (Utils.is_none prev_lt_opt) then
	      (Some (cur_lt), Some (cur_eq))
	    else
	      let prev_lt = Utils.elem_from_opt prev_lt_opt in
	      let prev_and = Utils.elem_from_opt prev_and_opt in
	      (Some (Ast.Or (dummy_loc, prev_lt, Ast.And (dummy_loc, prev_and, cur_lt))), Some (Ast.And (dummy_loc, prev_and, cur_eq)))
	  in
	  let (ordering_opt,_) = List.fold_left2 single_ordering (None, None) start_tuple end_tuple in
	  assert (Utils.is_some ordering_opt);
          Utils.elem_from_opt ordering_opt
	in
	let rev_instrs = List.tl rev_list in
	Ast.Implies (dummy_loc, start_ann, wp ordering_expr rev_instrs)
      end
  in
  vc_to_return ;;




let string_of_vc vc = Ast.string_of_expr vc ;;

(* Print a verification condition. *)
let print_vc vc =
  print_endline (string_of_vc vc) ;;

