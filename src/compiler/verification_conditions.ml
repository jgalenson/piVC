(* PiVC *)

open Basic_paths
open Expr_utils
open Ast

exception InvalidPath of string ;;

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
    | _ -> assert(false) ;;



let get_corresponding_class_decl var_ident program = 
  let looking_for =
    let looking_for_type = (varDecl_of_identifier var_ident).varType in
      match looking_for_type with
          Identifier(id,loc) -> id.name
        | _ -> assert(false)
  in
  let got_it what = 
    match what with
        ClassDecl (loc, c) -> c.className.name=looking_for
      | _ -> false
  in
    try
      match List.find got_it program.decls with
          ClassDecl(loc,c) -> c
        | _ -> assert(false)
    with Not_found -> assert(false)
      
let get_member_decl object_var program = 
  match object_var with 
      InsideObject(loc,id1,id2) ->
        begin
          let class_decl = get_corresponding_class_decl id1 program in
          let got_it what = 
            match what with
                VarDecl(loc,v) -> v.varName.name=id2.name
              | _ -> false
          in
            try
              List.find got_it class_decl.members
            with Not_found -> assert(false)
        end
    | _ -> assert(false)
            

let get_new_object_var_identifier object_var program = 
  match object_var with 
      InsideObject(loc,id1,id2) ->
        begin
          let new_ident_name = id1.name ^ "." ^ id2.name in
          let new_ident_location = location_union id1.location_id id2.location_id in
          let new_ident_identifier = create_identifier new_ident_name new_ident_location in
          let new_ident_id = "__" ^ id_of_identifier id1 ^ "_" ^ id2.name in
            
          let member_varDecl =
            let member_decl = get_member_decl object_var program in 
              match member_decl with
                  VarDecl(loc, vd) -> vd
                | _ -> assert(false)
          in
            
          let vd = create_varDecl member_varDecl.varType new_ident_identifier member_varDecl.location_vd
          in
            vd.var_id := Some(new_ident_id);
            let ident = create_identifier new_ident_name new_ident_location in
              ident.decl := Some(vd);
              ident
        end
    | _ -> assert(false)


let get_new_length_expr expr = 
  Ast.LValue(Ast.gdl(),Ast.NormLval(Ast.gdl(),get_new_length_identifier expr)) ;;

let replace_length_and_members_with_var expr program = 
  let rec rlwv_lval l = match l with
      NormLval(loc,id) -> l
    | ArrayLval(loc,exp1,exp2) -> ArrayLval(loc,rlwv exp1, rlwv exp2)
    | InsideObject(loc,id1,id2) -> NormLval(loc,get_new_object_var_identifier l program)
  and rlwv expr = match expr with
      Assign(loc,f,t) -> Assign(loc,rlwv_lval f,rlwv t)
    | Constant (loc,c) -> expr
    | LValue (loc,l) -> LValue(loc, rlwv_lval l)
    | Call (loc,s, el) -> (*calls should have been stripped previously, but there may still be predicates*)
        begin
          let new_args = List.map rlwv el in
            Call(loc,s,new_args)
        end
    | NewArray(loc, t, e) -> assert(false) (*should have been stripped out previously*)
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
    | Length (loc, t) -> get_new_length_expr (Length(loc, rlwv t))
    | EmptyExpr  -> expr
  in
  rlwv expr ;;

(* Converts all length() nodes to specially named idents.
   Adds a length assignment step to complement each array assignment step.
*)
let convert_basic_path_to_acceptable_form path program = 
  let replace_length_and_members_with_vars_in_step step = 
    match step with
        Basic_paths.Expr(e) -> Basic_paths.Expr(replace_length_and_members_with_var e program)
      | Assume(e) -> Assume(replace_length_and_members_with_var e program)
      | Annotation(e,s) -> Annotation(Ast.create_annotation_copy (replace_length_and_members_with_var e.ann program) e, s)
      | RankingAnnotation (ra) -> RankingAnnotation ( Ast.create_ranking_annotation_copy (List.map (function e -> replace_length_and_members_with_var e program) ra.tuple) ra)
  in
  let rec add_extra_node_for_length_if_necessary node =
    begin
      let new_node = replace_length_and_members_with_vars_in_step node in
        match new_node with
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
                                          [Basic_paths.Expr(Ast.Assign(Ast.gdl(),lhs,rhs));new_node]
                                      end
                                  | Ast.Identifier(class_id,loc)->
                                      begin
                                        let lhs_id = id in
                                        let rhs_id = match expr with
                                            Ast.LValue(loc,l) ->
                                              begin
                                                match l with
                                                    Ast.NormLval(loc, id) -> id
                                                  | _ -> assert(false)
                                              end
                                          | _ -> assert(false)
                                        in
                                          
                                        let class_decl = get_corresponding_class_decl lhs_id program in
                                        let gen_assign_for_member member =
                                          match member with
                                              VarDecl(l, vd) ->
                                                begin
                                                  let lhs_of_assign = InsideObject(gdl(),lhs_id,vd.varName) in
                                                  let rhs_of_assign = LValue(gdl(),InsideObject(gdl(),rhs_id,vd.varName)) in
                                                  let assign = Assign(gdl(), lhs_of_assign, rhs_of_assign) in
                                                    let new_node = Basic_paths.Expr(replace_length_and_members_with_var assign program) in
                                                    let new_resulting_nodes = add_extra_node_for_length_if_necessary new_node in
                                                      [new_node]@new_resulting_nodes
                                                end
                                            | _ -> assert(false)
                                        in
                                          List.flatten (List.map gen_assign_for_member class_decl.members)
                                      end
                                  | _ -> [new_node]
                              end
                          | _ -> [new_node]
                      end
                  | _ -> [new_node]
              end
          | _ -> [new_node]
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
            | InsideObject(loc,id1,id2) -> []
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
    | NewArray (loc, t, e) -> assert(false)
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
let get_vc bp program =

  let path_with_length_nodes = get_steps_from_path bp in
  let is_termination_path = Basic_paths.is_termination_path bp in
  let path = convert_basic_path_to_acceptable_form path_with_length_nodes program in
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
                        Ast.NormLval(loc,id) -> sub_idents_in_expr_while_preserving_original_location formula [(id, e)]
                      | Ast.ArrayLval(loc,arr,index) ->
                          begin
                            let array_ident = array_ident_from_lval l in
                            let array_update = array_write_to_array_update (Ast.LValue(Ast.get_dummy_location(),l)) e in
                              match array_update with
                                  Ast.Assign(loc,update_from,update_to) -> sub_idents_in_expr_while_preserving_original_location formula [(array_ident,update_to)]
                                | _ -> assert(false)
                          end
                      | _ -> assert(false)
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
      Ast.Implies (dummy_loc, start_ann.ann, wp end_ann.ann rev_instrs)
    else
      (* Handle termination basic paths. *)
      begin
	let get_tuple n = match n with
	  | RankingAnnotation (ra) -> ra.tuple
	  | _ -> raise (InvalidPath "No RankingAnnotation where expected in path")
	in
	let start_tuple = get_tuple (List.hd (List.tl path)) in
	(* Replaces all of the variable names in the start tuple with temp
	   ones whose names start with an underscore. *)
	let replaced_start_tuple =
	  let replace_expr e = 
	    let rename_var ident =
	      let loc = ident.location_id in
	      let new_id = { name = "_" ^ ident.name ; location_id = loc ; decl = ident.decl ; is_length = ident.is_length } in
	      Some (Ast.LValue (loc, NormLval (loc, new_id)))
	    in
	      Expr_utils.sub_idents e rename_var
	  in
	    List.map replace_expr start_tuple
	in
	let rev_list = List.rev (List.tl (List.tl path)) in
	let end_tuple = get_tuple (List.hd rev_list) in
	assert ((List.length start_tuple) = (List.length end_tuple));
	(* Get the Ast representation of < in the lexographic ordering of the two tuples. *)
	let ordering_expr =
	  (* Gets a single end<start ordering and combine it with the prev ones
	     so we can use List.fold_left to build up the full one.
	     We need to both keep the previous < orderings and build up a large AND of equals. *)
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
	  let (ordering_opt,_) = List.fold_left2 single_ordering (None, None) replaced_start_tuple end_tuple in
	  assert (Utils.is_some ordering_opt);
          Utils.elem_from_opt ordering_opt
	in
	let rev_instrs = List.tl rev_list in
	(* Get the weakest precondition. *)
	let weakest_precon = wp ordering_expr rev_instrs in
	(* Rename all variables starting with an _ back to their original name. *)
	let replaced_wp =
	  let rename_var ident =
	    if ident.name.[0] != '_' then
	      None
	    else
	      let loc = ident.location_id in
	      let new_id = { name = String.sub ident.name 1 ((String.length ident.name) - 1)  ; location_id = loc ; decl = ident.decl ; is_length = ident.is_length } in
	      Some (Ast.LValue (loc, NormLval (loc, new_id)))
	  in
	    Expr_utils.sub_idents weakest_precon rename_var
	in
	Ast.Implies (dummy_loc, start_ann.ann, replaced_wp)
      end
  in
  vc_to_return ;;

let string_of_vc vc = Ast.string_of_expr vc ;;

(* Print a verification condition. *)
let print_vc vc =
  print_endline (string_of_vc vc) ;;

