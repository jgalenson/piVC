(* PiVC *)

open Basic_paths ;;

exception InvalidPath of string ;;

(* Returns (last_elem, rest of list) of a list.
   E.g. remove_last_elem [1, 2, 3, 4] = (4, [1, 2, 3]) *)
let remove_last_elem l = 
  let rev_list = List.rev l in
  (List.hd rev_list, List.rev (List.tl rev_list)) ;;

(* Gets the Ast.Expr out of a path node. *)
let get_expr_from_path_node n = match n with
  | Annotation (e, _) -> e
  | _ -> raise (InvalidPath "No Annotation where expected in path") ;;  

(* Gets a VC out of a basic path.
   Returns the VC as an Ast.Expr. *)
let get_vc path =
  
  let dummy_loc = Ast.get_dummy_location () in
  let start_ann = get_expr_from_path_node (List.hd path) in
  let (end_node, instructions) = remove_last_elem (List.tl path) in
  let end_ann = get_expr_from_path_node end_node in

  (* Weakest precondition on a list: wp(formula, i1; i2; ...; in) *)
  let rec wp formula instrs =

    (* Weakest precondition on a single instruction: wp(formula, instr) *)
    let single_wp formula instr =
      match instr with
      | Assume (e) ->
	  Ast.Implies (dummy_loc, e, formula)
      | Expr (e) ->
	  (* TODO: Substitute.  Do same way as in basic paths? *)
	  Ast.Implies (dummy_loc, e, formula)
      | _ -> raise (InvalidPath "Annotation in middle of path")
    in
    
    if List.length instrs = 0 then
      raise (InvalidPath "No instructions in path")
    else if List.length instrs = 1 then
      single_wp formula (List.hd instrs)
    else
      let (last_instr, rest_of_instrs) = remove_last_elem instrs in
      wp (single_wp formula last_instr) rest_of_instrs
  in
  
  Ast.Implies (dummy_loc, start_ann, wp end_ann instructions) ;;

(* Print a verification condition. *)
let print_vc vc =
  print_endline (Ast.string_of_expr vc) ;;

