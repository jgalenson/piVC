(* PiVC *)

open Basic_paths
open Expr_utils
open Ast

exception InvalidPath of string ;;

(* Gets the Ast.Expr out of a path node. *)
let get_expr_from_path_node n = match n with
  | Annotation (e, _) -> e
  | _ -> raise (InvalidPath "No Annotation where expected in path") ;;  

(* Converts all length() nodes to specially named idents.
   Adds a length assignment step to complement each array assignment step.
*)
let go_length_operations path = 
  let get_new_length_identifier expr = 
    match expr with 
        Length(loc,exp) -> 
          begin
            let ident = identifier_of_array_expr exp in
            let new_ident = Ast.create_identifier ("|" ^ ident.name ^ "|") (gdl()) in
            let vd = Ast.create_varDecl (Int(gdl())) new_ident (gdl()) in
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
        Constant (loc,c) -> expr
      | LValue (loc,l) -> expr
      | Call (loc,s, el) -> assert(false);
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
      | Assign(loc,f,t) -> Assign(loc,f,rlwv t)
    in
      rlwv expr
  in
  let replace_length_with_var_in_step step = 
    match step with
        Basic_paths.Expr(e) -> Basic_paths.Expr(replace_length_with_var e)
      | Assume(e) -> Assume(replace_length_with_var e)
      | Annotation(e,s) -> Annotation(replace_length_with_var e, s)          
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
        

(* Gets a VC out of a basic path.
   Returns the VC as an Ast.Expr. *)
let get_vc path_with_length_nodes =

  let path = go_length_operations path_with_length_nodes in
  let dummy_loc = Ast.get_dummy_location () in
  let start_ann = get_expr_from_path_node (List.hd path) in
  let rev_list = List.rev (List.tl path) in
  let end_ann = get_expr_from_path_node (List.hd rev_list) in
  let rev_instrs = List.tl rev_list in

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
                        Ast.NormLval(loc,id) -> sub_idents_in_expr formula [(Ast.string_of_lval l, e)]
                      | Ast.ArrayLval(loc,arr,index) ->
                          begin
                            match array_name_from_lval l with
                                Some(name) ->
                                  begin
                                    let array_update = array_write_to_array_update (Ast.LValue(Ast.get_dummy_location(),l)) e in
                                      match array_update with
                                          Ast.Assign(loc,update_from,update_to) -> sub_idents_in_expr formula [(name,update_to)]
                                        | _ -> assert(false)
                                  end
                                    (*TODO-A: possibly clean up the above two lines...possibly use update_from instead of name*)
                              | None -> formula
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
  let vc_to_return = Ast.Implies (dummy_loc, start_ann, wp end_ann rev_instrs) in
    vc_to_return ;;




let string_of_vc vc = Ast.string_of_expr vc ;;

(* Print a verification condition. *)
let print_vc vc =
  print_endline (string_of_vc vc) ;;

