open Ast ;;

(* Given a Call expr, returns the identifier of the function it calls. *)
let get_called_function call = match call with
  | Call (_, id, _) -> id
  | _ -> assert false (* We should only ever call this on a Call expr. *) ;;

(* Gets the decl that this Call expr is calling. *)
let get_called_fndecl calling_fndecl program call =
  let called_fn_name = get_called_function call in
  let find_decl d = match d with
    | FnDecl (_, fd) ->
	if (string_of_identifier called_fn_name) = (string_of_identifier fd.fnName) then
	  Some fd
	else
	  None
    | _ -> None
  in
  let is_called_fn d =
    Utils.is_some (find_decl d)
  in
  try
    Some (List.find is_called_fn program.decls)
  with
      Not_found -> None ;;

(* Gets the fnDecl from a call.
   We return it as an option since there could be an error. *)
let get_fndecl_from_call get_fndecl c =
  let lookup_result = get_fndecl c in
  match lookup_result with
    | None -> None
    | Some (d) -> begin
	match d with
	  | FnDecl (_, fd) -> Some (fd)
	  | _ -> None
      end ;;

(* Gets all the functions func calls.
   We return one Call expr for each function we call. *)
let get_fn_calls func get_fndecl =
  let rec get_calls_from_stmt s = match s with
    | Expr (_, e) -> [Expr_utils.get_calls e]
    | StmtBlock (_, st) -> List.concat (List.map get_calls_from_stmt st)
    | IfStmt (_, test, s1, s2) -> [Expr_utils.get_calls test] @ (get_calls_from_stmt s1) @ (get_calls_from_stmt s2)
    | WhileStmt (_, test, block, _, _) -> [Expr_utils.get_calls test] @ get_calls_from_stmt block
    | ForStmt (_, init, test, incr, block, _, _) -> [Expr_utils.get_calls init] @ [Expr_utils.get_calls test] @ [Expr_utils.get_calls incr] @ get_calls_from_stmt block
    | ReturnStmt (_, e) -> [Expr_utils.get_calls e]
    | _ -> []
  in
  let calls_per_line = get_calls_from_stmt func.stmtBlock in
  let all_calls = List.concat calls_per_line in
  let make_unique prev cur =
    let equals x =
      x == cur
    in
    if (List.exists equals prev) then
      prev
    else
      prev @ [cur]
  in
  List.fold_left make_unique [] all_calls ;;

(* Gets a list of all the loops in a function. *)
let get_loops func =
  let rec get_a_loop s = match s with
    | StmtBlock (_, sl) -> List.concat (List.map get_a_loop sl)
    | IfStmt (_, _, s1, s2) -> (get_a_loop s1) @ (get_a_loop s2)
    | WhileStmt (_, _, block, _, _) -> [s] @ (get_a_loop block)
    | ForStmt (_, _, _, _, block, _, _) -> [s] @ (get_a_loop block)
    | _ -> []
  in
  let list_of_loops = get_a_loop func.stmtBlock in
  list_of_loops ;;

(* Gets the ranking annotation out of a loop. *)
let rec get_loop_ranking_annotation s = match s with
  | WhileStmt (_, _, _, _, ra) -> ra
  | ForStmt (_, _, _, _, _, _, ra) -> ra
  | _ -> assert false (* We should only call this on loops. *) ;;

module Fndecl_set = Set.Make (struct
				type t = fnDecl
				let compare a b =
				  let a_name = Ast.unique_fn_name a in
				  let b_name = Ast.unique_fn_name b in
				  String.compare a_name b_name
                              end)
(* Checks whether one function calls another. *)
let calls calling_func callee_func get_fndecl =
  let visited = ref Fndecl_set.empty in
  (* Checks whether caller calls callee. *)
  let rec does_call caller callee depth =
    let are_same_fn f1 f2 =
      (Ast.unique_fn_name f1) == (Ast.unique_fn_name f2)
    in
    if (are_same_fn callee caller && depth > 0) then
      true
    else if (Fndecl_set.mem caller !visited) then
      false
    else begin
      visited := Fndecl_set.add caller !visited;
      let all_calls = get_fn_calls caller get_fndecl in
      let all_calls_decls = 
	let decl_opt_list = List.map (get_fndecl_from_call get_fndecl) all_calls in
	let some_decl_list = List.filter Utils.is_some decl_opt_list in
	List.map Utils.elem_from_opt some_decl_list
      in
      let map_fn x = does_call x callee (depth + 1) in
      List.exists map_fn all_calls_decls
    end
  in
  does_call calling_func callee_func 0 ;;
