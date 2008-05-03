(* piVC *)

open Parser
open Ast
open Semantic_checking
open Background
open Utils ;;

type validity = Valid | Invalid | Unknown;;

exception Dp_server_exception of string ;;
exception Malformatted_DP_Server_Address;;

type termination_result = {
  overall_validity : validity;
  decreasing_paths : validity * (Basic_paths.basic_path * Ast.expr * validity * Counterexamples.example list option) list;
  nonnegative_vcs : validity * (Ast.expr * validity * Counterexamples.example list option) list;
} ;;

let create_termination_result ov paths nvcs =
  { overall_validity = ov; decreasing_paths = paths; nonnegative_vcs = nvcs; } ;;

let string_of_validity v = match v with
  | Valid -> "valid"
  | Invalid -> "invalid"
  | Unknown -> "unknown" ;;

let is_valid v = match v with
  | Valid -> true
  | _ -> false ;;

let is_unknown v = match v with
  | Unknown -> true
  | _ -> false ;;

let overall_validity validities =
  if (List.for_all is_valid validities) then
    Valid
  else if (List.exists is_unknown validities) then
    Unknown
  else
    Invalid ;;

let instantiate_predicates expr program = 
  let get_predicate pred_ident = 
    let is_match decl = 
      match decl with
          Predicate(loc,p) -> p.predName.name=pred_ident.name
        | _ -> false
    in
      try
        let pred = List.find is_match program.decls
        in
          match pred with
              Predicate(loc,p) -> p
            | _ -> assert(false)
      with ex -> assert(false)
  in
  let rec ip expr = 
    match expr with
        Assign(loc,f,t) -> Assign(loc,f,ip t)
      | Constant (loc,c) -> expr
      | LValue (loc,l) -> expr
      | Call (loc,s, el) ->
          begin
            let new_el = List.map ip el in
            let pred_decl = get_predicate s in
            let get_replacement_pair decl arg = 
              (decl.varName,arg) in
            let replacement_pairs = List.map2 get_replacement_pair pred_decl.formals_p new_el in
              Expr_utils.sub_idents_in_expr pred_decl.expr replacement_pairs
          end
      | Plus (loc,t1, t2) -> Plus(loc,ip t1, ip t2)
      | Minus (loc,t1, t2) -> Minus(loc,ip t1, ip t2)
      | Times (loc,t1, t2) -> Times(loc,ip t1, ip t2)
      | Div (loc,t1, t2) -> Div(loc,ip t1, ip t2)
      | IDiv (loc,t1, t2) -> IDiv(loc,ip t1, ip t2)
      | Mod (loc,t1, t2) -> Mod(loc,ip t1, ip t2)
      | UMinus (loc,t) -> UMinus(loc,ip t)
      | ForAll (loc,decls,e) -> ForAll(loc,decls,ip e)
      | Exists (loc,decls,e) -> Exists(loc,decls,ip e)
      | ArrayUpdate (loc,exp,assign_to,assign_val) -> ArrayUpdate(loc,ip exp, ip assign_to, ip assign_val)
      | LT (loc,t1, t2) -> LT(loc,ip t1, ip t2)
      | LE (loc,t1, t2) -> LE(loc,ip t1, ip t2)
      | GT (loc,t1, t2) -> GT(loc,ip t1, ip t2)
      | GE (loc,t1, t2) -> GE(loc,ip t1, ip t2)
      | EQ (loc,t1, t2) -> EQ(loc,ip t1, ip t2)
      | NE (loc,t1, t2) -> NE(loc,ip t1, ip t2)
      | And (loc,t1, t2) -> And(loc,ip t1, ip t2)
      | Or (loc,t1, t2) -> Or(loc,ip t1, ip t2)
      | Not (loc,t) -> Not(loc,ip t)
      | Iff (loc,t1, t2) -> Iff(loc,ip t1, ip t2)
      | Implies (loc,t1, t2) -> Implies(loc,ip t1, ip t2)
      | Length (loc, t) -> assert(false);
      | EmptyExpr  -> expr        
  in
  let result = ip expr in
    result


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
  if (Config.get_value_int "cache_size") = 0 then
    ()
  else
    begin
      if (Hashtbl.length cache) = (Config.get_value_int "cache_size") then
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
let verify_vc (vc_with_preds, (vc_cache, cache_lock), program) =
  try
    let vc = instantiate_predicates vc_with_preds program in
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
      (*let vc_no_quants = Expr_utils.remove_quantification_from_vc_with_array_dp vc in*)
      (*let vc_no_quants_array_lengths_geq_0 = (*Verification_conditions.add_array_length_greater_than_0_to_expr*) vc_no_quants in*)
      (*let negated_vc_no_quants_array_lengths_geq_0 = (Not (get_dummy_location (), vc_no_quants_array_lengths_geq_0)) in*)

      let final_vc = Expr_utils.remove_quantification_from_vc_with_array_dp (Not (get_dummy_location (), (Verification_conditions.add_array_length_greater_than_0_to_expr vc))) in


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

      let (vc, rev_var_names) = Transform_yices.transform_for_yices final_vc in
      let (sock, inchan, outchan) =
        let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
        let server_addr_and_port = Config.get_value "dp_server_address" in
          begin
            try
              let index_of_colon = String.index server_addr_and_port ':' in
              let server_addr = String.sub server_addr_and_port 0 index_of_colon in
              let server_port = int_of_string (String.sub server_addr_and_port (index_of_colon+1) ((String.length server_addr_and_port)-index_of_colon-1)) in
                Unix.connect sock (Unix.ADDR_INET(Unix.inet_addr_of_string server_addr, server_port))
            with
                Invalid_argument(_) -> raise Malformatted_DP_Server_Address
              | Not_found -> raise Malformatted_DP_Server_Address
              | Failure("int_of_string") -> raise Malformatted_DP_Server_Address
          end
          ;
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
            else if (response = "sat") then
	      begin
	        let counterexample_str = Net_utils.get_input inchan in
		let counterexample = Counterexamples.parse_counterexample counterexample_str rev_var_names in
	        (Invalid, Some (counterexample))
	      end
	    else if (response = "error") then
	      begin
	        let error_msg = Net_utils.get_input inchan in
	          raise (Dp_server_exception error_msg)
	      end
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
let verify_program program_info program_ast vc_cache_and_lock =

  (* First, build an intermediate structure that
     contains a Background process for each VC.
     That is, we start all the VC requests. *)
  let intermediate_info =
    let get_vc_thread vc = Background.create verify_vc (vc, vc_cache_and_lock, program_ast) in
    let intermediate_basic_path (path, vc) =
      let vc_thread = get_vc_thread vc in
      (path, vc, vc_thread)
    in
    let intermediate_nonnegative_vc nvc =
      let vc_thread = get_vc_thread nvc in
      (nvc, vc_thread)
    in
    (* Makes a data structure where we replace each VC expr with VC expr * VC background thread. *)
    let intermediate_fn (fn, norm_bps, term_bps, nvcs) =
      let norm_path = List.map intermediate_basic_path norm_bps in
      let term_path = List.map intermediate_basic_path term_bps in
      let nvc = List.map intermediate_nonnegative_vc nvcs in
      (fn, norm_path, term_path, nvc)
    in
    List.map intermediate_fn program_info
  in

  (* Verify a function.
     Takes in a tuple of its name, its normal basic paths, its
     termination basic paths, and the non-negative VCs.
     We first call the verify_* fns to start all the background VC requests.
     We then get all of the results and get the overall validities. *)
  let rec verify_function (fn, norm_paths, term_paths, nvcs) =
    (* Gets the result of a thread and either returns it or
       raises the exception that the thread raised. *)
    let get_vc_result vc_thread = 
      let vc_result = Background.get_result vc_thread in
	match vc_result with
	  | Normal (v, c) -> (v, c)
	  | Exceptional (e) -> raise e
    in
    let verify_basic_path (path, vc, vc_thread) =
      let (v, c) = get_vc_result vc_thread in
      (path, vc, v, c) 
    in
    let verify_nonnegative_vc (nvc, vc_thread) =
      let (v, c) = get_vc_result vc_thread in
      (nvc, v, c)
    in
    let verified_norm_paths = List.map verify_basic_path norm_paths in
    let verified_term_paths = List.map verify_basic_path term_paths in
    let validity_of_path (path, vc, validity, count) = validity in
    let overall_norm_path_status = overall_validity_status verified_norm_paths validity_of_path in
    let overall_term_path_status = overall_validity_status verified_term_paths validity_of_path in
    let norm_path_info = (overall_norm_path_status, verified_norm_paths) in
    let (termination_result, overall_term_validity) =
      if (List.length term_paths > 0) then
	let term_path_info = (overall_term_path_status, verified_term_paths) in
	let verified_nvcs = List.rev_map verify_nonnegative_vc nvcs in
	let validity_of_nvc (nvc, validity, counterexample) = validity in
	let overall_nvc_status = overall_validity_status verified_nvcs validity_of_nvc in
	let overall_term_status = overall_validity [overall_term_path_status; overall_nvc_status] in
        let termination_info = create_termination_result overall_term_status term_path_info (overall_nvc_status, verified_nvcs) in
	(Some (termination_info), overall_term_status)
      else
	(None, Valid)
    in
    let overall_fn_status = overall_validity [overall_norm_path_status; overall_term_validity] in
    (fn, overall_fn_status, norm_path_info, termination_result)
  in

  let verified_functions = List.map verify_function intermediate_info in
  let validity_of_function (name,validity,norm_bp_info,term_info) = validity in
    (overall_validity_status verified_functions validity_of_function, verified_functions)

(* Gets all the info we need from a program.
   That is, for each method, its basic paths and VCs: (path_node list * expr).list
   Returns (fn * (Normal Basic Path * VC) list * (Term basic path * VC list) * non-negative VC list) list. *)
let get_all_info program gen_runtime_asserts =
  (* Returns a list of pairs of fnName and its basic path.
     Returns (fn * (basic_path list * (norm_basic_path list, term_basic_path list))) list. *)
  let get_basic_paths program =
    let get_decl_paths_if_appropriate decl = 
      match decl with
          VarDecl (loc, vd) -> None
        | Predicate (loc, p) -> None
	| FnDecl (loc, fd) -> (Some (fd, Basic_paths.generate_paths_for_func fd program gen_runtime_asserts))
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
  (* Gets the VCs for each basic path. *)
  let get_vcs (fnName, (normal_paths, termination_paths)) =
    let norm_vcs = List.map (fun path -> (path, Verification_conditions.get_vc path)) normal_paths in
    let term_vcs = List.map (fun path -> (path, Verification_conditions.get_vc path)) termination_paths in
    let nonneg_vcs = Termination.get_nonnegativity_vcs program in
    (fnName, norm_vcs, term_vcs, nonneg_vcs)
  in
  let paths = get_basic_paths program in
  List.map get_vcs paths ;;
