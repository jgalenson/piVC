(* PiVC *)

open Basic_paths
open Expr_utils

exception InvalidPath of string ;;
exception UnexpectedExpr

(* Gets the Ast.Expr out of a path node. *)
let get_expr_from_path_node n = match n with
  | Annotation (e, _) -> e
  | _ -> raise (InvalidPath "No Annotation where expected in path") ;;  

(* Gets a VC out of a basic path.
   Returns the VC as an Ast.Expr. *)
let get_vc path =
  
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
      | Expr (exp) ->
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
                                        | _ -> raise UnexpectedExpr
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

