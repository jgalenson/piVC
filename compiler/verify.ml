(* piVC *)

open Parser
open Ast
open Semantic_checking
open Utils ;;

let verify_vc vc =
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let server_addr = (Unix.inet_addr_of_string Constants.dp_server_address) in
  Unix.connect sock (Unix.ADDR_INET(server_addr, Constants.dp_server_port));
  let inchan = Unix.in_channel_of_descr sock in
  let outchan = Unix.out_channel_of_descr sock in
  let vc_yices_string = Transform_yices.get_yices_string vc in
  Net_utils.send_output outchan vc_yices_string;
  flush outchan;
  let response = Net_utils.get_input inchan in  
  Unix.close sock;
  if (response = "unsat") then
    true
  else if (response = "sat") then
    false
  else
    assert false
  
(* Returns (fn * bool * (Basic Path * VC * bool) list) list. *)
let verify_program program_info = 
  let rec verify_function func = 
    let verified_basic_paths = List.map verify_basic_path (snd func) in
    let path_is_pass (path, vc, pass) = pass in
    let all_paths_passed = List.for_all path_is_pass verified_basic_paths in
    (fst func, all_paths_passed, verified_basic_paths)
  and verify_basic_path path_info =
    (* TODO: Rewrite so we make only one request to server. *)
    (fst path_info, snd path_info, verify_vc (snd path_info))
  in 
  let verified_functions = List.map verify_function program_info in
  let func_is_pass (name,pass,info) = pass in
  let all_funcs_passed = List.for_all func_is_pass verified_functions in
    (all_funcs_passed, verified_functions)

(* Gets all the info we need from a program.
   That is, for each method, its basic paths and VCs: (path_node list * expr).list
   Returns (fn * (Basic Path * VC) list) list. *)
let get_all_info program =
  (* Returns a list of pairs of fnName and its basic path.
     Returns (fn * path_node list list) list. *)
  let get_basic_paths program =
    let get_decl_paths_if_appropriate decl = 
      match decl with
          VarDecl (loc, vd) -> None
	| FnDecl (loc, fd) -> (Some (fd, Basic_paths.generate_paths_for_func fd program))
    in
    (* Concatenate together functions ignoring vardecls. *)
    let map_fn all cur =
      let paths = get_decl_paths_if_appropriate cur in
	match paths with
	    None -> all
	  | Some (e) -> all @ [e]
    in
      List.fold_left map_fn [] program.decls
  in
  (* Add the VCs into the fnName + Basic path info. *)
  let get_vcs (fnName, paths) =
    (fnName, List.map (fun path -> (path, Verification_conditions.get_vc path)) paths)
  in
  let paths = get_basic_paths program in
  List.map get_vcs paths ;;

