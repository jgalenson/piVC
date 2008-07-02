open Ast ;;

(* Data structures *)

(* Types of exprs that are in the cfg. *)
type cfg_expr =
  | NormExpr of expr
  | NormAnnotation of annotation
  | Entry
  | Exit ;;

(* Orders cfg_expressions based on their locations.
   We order entry first, then normal exprs and annotations,
   and then exit, *)
let compare_cfg_exprs e1 e2 =
  let compare_exprs e1 e2 = 
    let loc1 = (location_of_expr e1).loc_start in
    let loc2 = (location_of_expr e2).loc_start in
    Utils.compare_locs loc1 loc2
  in
  match (e1, e2) with
    | (Entry, Entry) -> 0
    | (Exit, Exit) -> 0
    | (NormAnnotation (a1), NormAnnotation (a2)) -> compare_exprs a1.ann a2.ann
    | (NormExpr (e1), NormExpr (e2)) -> compare_exprs e1 e2
    | (NormExpr (e), NormAnnotation (a)) -> compare_exprs e a.ann
    | (NormAnnotation (a), NormExpr (e)) -> compare_exprs a.ann e
    | (Entry, _) -> -1
    | (_, Entry) -> 1
    | (Exit, _) -> 1
    | (_, Exit) -> -1 ;;

module Cfg_expr_map = Map.Make (struct
				  type t = cfg_expr
				  let compare e1 e2 = compare_cfg_exprs e1 e2
				end)

module Cfg_expr_set = Set.Make (struct
				  type t = cfg_expr
				  let compare e1 e2 = compare_cfg_exprs e1 e2
				end)

(* A function cfg node. *)
type function_cfg = {
  fn : fnDecl;
  nodes : cfg_expr list;
  entry : cfg_expr;
  exit : cfg_expr;
  predecessor_map : cfg_expr list Cfg_expr_map.t; (* Maps an expr to its predecessors. *)
  successor_map : cfg_expr list Cfg_expr_map.t; (* Maps an expr to its successors. *)
} ;;

(* The cfg. *)
type cfg = {
  functions : function_cfg list;
} ;;

(*
  TODO: Is this the right way to deal with exprs inside of stmts (e.g. test in If, While, For)?
*)
(* Creates the cfg for a program. *)
let make_cfg prog =
  (* Makes a function cfg node.
     d is a decl, and we of course only do stuff for fnDecls. *)
  let make_function_cfg d =
    let pred_map = ref Cfg_expr_map.empty in
    let succ_map = ref Cfg_expr_map.empty in
    let nodes = ref Cfg_expr_set.empty in
    let entry = Entry in
    let exit = Exit in
    (* Wires it up so that prev is a predecessor of cur and
       cur is a successor of prev. *)
    let wire_up cur_expr prev_exprs =
      (* Adds a key->value mapping to the specified map. *)
      let add_to_map key value map =
	let q =
	  if Cfg_expr_map.mem key !map then
	    Cfg_expr_map.find key !map
	  else
	    Queue.create ()
	in
	Queue.add value q;
	map := Cfg_expr_map.add key q !map;
	nodes := Cfg_expr_set.add key !nodes
      in
      List.iter (fun prev -> add_to_map cur_expr prev pred_map) prev_exprs;
      List.iter (fun prev -> add_to_map prev cur_expr succ_map) prev_exprs
    in
    (* Recursively create the function cfg.
       stmt is the stmt we are currently examining.
       prev_exprs is the cfg_exprs we have just looked at previously
       and that we want to wire up to the current expr.
       fn is the fnDecl and depth is the current depth within the function,
       which are both just for edge cases. *)
    let rec make_cfg stmt prev_exprs fn depth =
      match stmt with
	| Expr (_, e) ->
	    let expr = NormExpr(e) in
	    wire_up expr prev_exprs;
	    [ expr ]
	| IfStmt (_, test, s1, s2) ->
	    let test_expr = NormExpr (test) in
	    wire_up test_expr prev_exprs;
	    let last_child_if = make_cfg s1 [ test_expr ] fn (depth + 1) in
	    let last_child_then = make_cfg s2 [ test_expr ] fn (depth + 1) in
	    List.append last_child_if last_child_then
	| WhileStmt (_, test, block, ann, _) ->
	    let assert_annotation = NormAnnotation (ann) in
	    wire_up assert_annotation prev_exprs;
	    let test_expr = NormExpr (test) in
	    wire_up test_expr [ assert_annotation ];
	    make_cfg block [ test_expr ] fn (depth + 1)
	| ForStmt (_, init, test, incr, block, ann, _) ->
	    let init_expr = NormExpr (init) in
	    wire_up init_expr prev_exprs;
	    let assert_annotation = NormAnnotation (ann) in
	    wire_up assert_annotation [ init_expr ];
	    let test_expr = NormExpr (test) in
	    wire_up test_expr [ assert_annotation ];
	    let last_child = make_cfg block [ test_expr ] fn (depth + 1) in
	    let incr_expr = NormExpr (incr) in
	    wire_up incr_expr last_child;
	    wire_up assert_annotation [ incr_expr ];
	    [ test_expr ]
	| ReturnStmt (_, e) ->
	    let expr = NormExpr(e) in
	    wire_up expr prev_exprs;
	    wire_up (NormAnnotation fn.postCondition) [ expr ];
	    [ expr ]
	| AssertStmt (_, a) ->
	    let assert_annotation = NormAnnotation(a) in
	    wire_up assert_annotation prev_exprs;
	    [ assert_annotation ]
	| StmtBlock (_, st) ->
	    let fold_fn prev_exprs cur_stmt =
	      make_cfg cur_stmt prev_exprs fn (depth + 1)
	    in
	    let last_children = List.fold_left fold_fn prev_exprs st in
	    (* If the function is void and we are the top-level StmtBlock,
	       we must wire the last stmt up with the post condition. *)
	    if (depth == 0 && List.length st > 0 && is_void_type fn.returnType) then
	      wire_up (NormAnnotation fn.postCondition) last_children;
	    last_children
	| _ -> prev_exprs
    in
    match d with
      | FnDecl (_, fn) ->
	  (* Build the cfg. *)
	  let precondition = NormAnnotation fn.preCondition in
	  wire_up precondition [ entry ];
	  ignore (make_cfg fn.stmtBlock [ precondition ] fn 0);
	  wire_up exit [ NormAnnotation fn.postCondition ];
	  (* Convert data types to build the final data structure. *)
	  let nodes_list = Cfg_expr_set.fold (fun cur prev -> prev @ [cur]) !nodes [] in
	  let queue_map_to_list_map q =
	    Utils.queue_to_list q
	  in
	  let final_pred_map = Cfg_expr_map.map queue_map_to_list_map !pred_map in
	  let final_succ_map = Cfg_expr_map.map queue_map_to_list_map !succ_map in
	  [ { fn = fn; nodes = nodes_list; entry = entry; exit = exit; predecessor_map = final_pred_map; successor_map = final_succ_map } ]
      | _ -> []
  in
  let fn_cfgs_with_empty = List.map make_function_cfg prog.decls in
  (* Remove empty lists (from decls that are not functions). *)
  let fn_cfgs = List.concat fn_cfgs_with_empty in
  let prog_cfg = { functions = fn_cfgs } in
  prog_cfg ;;

(* Makes a string out of a cfg. *)
let string_of_cfg cfg =
  let string_of_fn_cfg fcfg =
    (* Make a string out of a cfg_expr that includes
       the expr itself and its predecessors and successors. *)
    let string_of_cfg_node e tab_str =
      (* Make a string out of a cfg_expr by just printing the expr. *)
      let string_of_cfg_expr e tab_str =
	let expr_str = match e with
	  | NormExpr (expr) -> string_of_expr expr
	  | NormAnnotation (ann) -> string_of_annotation ann
	  | Entry -> "Entry"
	  | Exit -> "Exit"
	in
	tab_str ^ expr_str
      in
      (* Makes a string out of a node's successors or predecessors. *)
      let get_node_string cexpr map =
	if Cfg_expr_map.mem cexpr map then
	  let nodes = Cfg_expr_map.find cexpr map in
	  let node_strs = List.map (fun x -> string_of_cfg_expr x (tab_str ^ "\t\t")) nodes in
	  String.concat "\n" node_strs
	else
	  ""
      in
      let pred_string = tab_str ^ "\t" ^ "Predecessors: \n" ^ get_node_string e fcfg.predecessor_map in
      let succ_string = tab_str ^ "\t" ^ "Successors: \n" ^ get_node_string e fcfg.successor_map in
      string_of_cfg_expr e tab_str ^ "\n" ^ pred_string ^ "\n" ^ succ_string ^ "\n"
    in
    let fn_name_str = string_of_identifier (fcfg.fn.fnName) in
    let expr_strs = List.map (fun x -> string_of_cfg_node x "\t") fcfg.nodes in
    let body_str = String.concat "\n" expr_strs in
    fn_name_str ^ "\n" ^ body_str ^ "\n" in
  let fn_strs = List.map string_of_fn_cfg cfg.functions in
  String.concat "\n" fn_strs ;;

(* Testing code: remove me *)
let _ =
  Config.set_server_type Config.Parser;
  Config.load (Utils.get_absolute_path Constants.main_server_config_file_path);
  (* Reads in the file. *)
  let filename = Sys.argv.(1) in
  let file = Unix.openfile filename [Unix.O_RDONLY] 0o640 in
  let file_size = (Unix.fstat file).Unix.st_size in
  let file_text =
    let file_text_temp = String.create file_size in
    ignore (Unix.read file file_text_temp 0 file_size);
    file_text_temp
  in
  (* Compile and generate cfg. *)
  let (program, errors) = Compile.parse_strings [(filename, file_text)] in
  if (Utils.is_some program) then
    let cfg = make_cfg (Utils.elem_from_opt program) in
    let cfg_str = string_of_cfg cfg in
    print_string cfg_str;
  
