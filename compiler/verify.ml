(* piVC *)

open Parser
open Ast
open Semantic_checking
open Background
open Utils ;;

type validity = Valid | Invalid | Unknown;;

let string_of_validity v = match v with
  | Valid -> "valid"
  | Invalid -> "invalid"
  | Unknown -> "unknown" ;;

(* The return type of verify_vc.
   Used so that the child thread can either return
   something we care about or an exception. *)
type vc_thread_response =
  | Normal of validity * Counterexamples.example list option
  | Exceptional of exn ;;

(* Evicts the oldest element in the cache.
   You must already hold the cache lock when calling this. *)
let evict_oldest_member vc_cache =
  let get_oldest_member k (_,t) (prev_k, prev_t) =
    if (prev_k = "" || t <= prev_t) then
      (k, t)
    else
      (prev_k, prev_t)
  in
  let (oldest_member,_) = Hashtbl.fold get_oldest_member vc_cache ("", Unix.time ()) in
  print_endline ("VC cache is evicting " ^ Utils.truncate_for_printing oldest_member);
  Hashtbl.remove vc_cache oldest_member ;;

(* Adds this vc to the cache.  If the cache is full,
   we first evict the oldest element.
   You must already hold the cache lock when calling this. *)   
let add_to_cache cache key result =
  if Constants.num_cached_vcs = 0 then
    ()
  else
    begin
      if (Hashtbl.length cache) = Constants.num_cached_vcs then
	begin
	  evict_oldest_member cache
	end;
      (* We use replace since someone might have added it in the meantime
	 when we didn't hold the lock. *)
      Hashtbl.replace cache key (result, Unix.time ())
    end ;;

(* Verifies a VC.
   Either returns the validity and a counterexample option
   or returns whatever exception was thrown. *)
let verify_vc (vc, (vc_cache, cache_lock)) =
  try
    (* Use cached version if we can. *)
    let unique_vc_str = Expr_utils.guaranteed_unique_string_of_expr vc in
    Mutex.lock cache_lock;
    if (Hashtbl.mem vc_cache unique_vc_str) then
      begin
        let (result,_) = Hashtbl.find vc_cache unique_vc_str in
        Hashtbl.replace vc_cache unique_vc_str (result, Unix.time ()); (* Update timestamp. *)
        Mutex.unlock cache_lock;
        print_endline ("Loaded from cache: " ^ Utils.truncate_for_printing unique_vc_str ^ " is " ^ (string_of_validity (fst result)));
        Normal (fst result, snd result)
      end
    else
      begin (* Otherwise, get answer from dp server. *)
      Mutex.unlock cache_lock;
      (*Note from Jason: do not remove any commented-out code from this file. I might need it for later debugging.*)
      (*let negated_vc = (Not (get_dummy_location (), vc)) in*)
      (*let negated_vc_nnf = Expr_utils.nnf (Not (get_dummy_location (), vc)) in*)
      let negated_vc_no_quants = Expr_utils.remove_quantification_from_vc_with_array_dp (Not (get_dummy_location (), vc)) in

(*  
  print_endline ("*********************************");
  print_endline ("VC in NNF is: \n" ^ string_of_expr negated_vc_nnf);
  print_endline ("Index set is as follows:");
  let print_expr exp = print_endline (Expr_utils.guaranteed_unique_string_of_expr exp) in
    List.iter print_expr (Expr_utils.get_index_set negated_vc_nnf);
  print_string ("Gave the following VC to yices: \n" ^ string_of_expr negated_vc_no_quants ^ "\n");
  (*print_string ("And got a response of: " ^ response ^ "\n");*)
  print_endline ("*********************************");
*) 

      let (vc, rev_var_names) = Transform_yices.transform_for_yices negated_vc_no_quants in
      let (sock, inchan, outchan) =
        let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
        let server_addr = Constants.dp_server_address in
        Unix.connect sock (Unix.ADDR_INET(server_addr, Constants.dp_server_port));
        let inchan = Unix.in_channel_of_descr sock in
        let outchan = Unix.out_channel_of_descr sock in
        (sock, inchan, outchan)
      in
      Net_utils.send_output outchan vc;
      flush outchan;
      let response = Net_utils.get_input inchan in  
      Unix.close sock;
      (* A VC is valid iff its negation is unsatisfiable. *)
      let result =
        if (response = "unsat") then
	  (Valid, None)
        else if (response = "unknown") then
	  (Unknown, None)
        else if (String.sub response 0 3 = "sat") then
	  (Invalid, Some (Counterexamples.parse_counterexamples response rev_var_names))
        else
	  assert false
      in
        Mutex.lock cache_lock;
        add_to_cache vc_cache unique_vc_str result;
        Mutex.unlock cache_lock;
        Normal (fst result, snd result)
      end
  with ex -> Exceptional (ex) ;;

let overall_validity_status list_of_things extraction_func = 
  let is_validity extraction_func test actual = (test==(extraction_func actual)) in
    if (List.for_all (is_validity extraction_func Valid) list_of_things) then Valid
    else if (List.exists (is_validity extraction_func Invalid) list_of_things) then Invalid
    else Unknown
;;

(* Returns (fn * bool * (Basic Path * VC * validity * example list option) list) list. *)
let verify_program program_info vc_cache_and_lock =

  (* First, build an intermediate structure that
     contains a Background process for each VC.
     That is, we start all the VC requests. *)
  let intermediate_info =
    let intermediate_basic_path (path, vc) =
      let vc_thread = Background.create verify_vc (vc, vc_cache_and_lock) in
      (path, vc, vc_thread)
    in
    let intermediate_fn (fn, bp) =
      let path = List.map intermediate_basic_path bp in
      (fn, path)
    in
    List.map intermediate_fn program_info
  in
  
  let rec verify_function func = 
    let verified_basic_paths = List.map verify_basic_path (snd func) in
    let validity_of_path (path, vc, validity, count) = validity in
    (fst func, overall_validity_status verified_basic_paths validity_of_path, verified_basic_paths)

  and verify_basic_path (path, vc, vc_thread) =
    let vc_result = Background.get_result vc_thread in
    match vc_result with
      | Normal (v, c) -> (path, vc, v, c)
      | Exceptional (e) -> raise e
  in 

  let verified_functions = List.map verify_function intermediate_info in
  let validity_of_function (name,validity,info) = validity in
    (overall_validity_status verified_functions validity_of_function, verified_functions)

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
