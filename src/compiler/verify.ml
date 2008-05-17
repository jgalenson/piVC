(* piVC *)

open Parser
open Ast
open Semantic_checking
open Background
open Utils
open Lexing

exception Dp_server_exception of string ;;
exception Malformatted_DP_Server_Address;;

type validity = Valid | Invalid | Unknown


and termination_result = {
  overall_validity_t : validity;
  decreasing_paths_validity : validity;
  nonnegative_vcs_validity : validity;
  decreasing_paths : vc_detailed list;
  nonnegative_vcs : vc_detailed list;
}
and correctness_result = {
  overall_validity_c : validity;
  vcs : vc_detailed list;
} 
and vc_detailed = {
  vc : vc_conjunct list list;
  bp : Basic_paths.basic_path option; (*nonnegative_vcs don't have basic paths*)
  valid : validity;
  counter_example : Counterexamples.example list option;
}
and vc_temp = {
  func_temp: fnDecl;
  vc_temp : vc_conjunct list list;
  bp_temp : Basic_paths.basic_path option;
}
and vc_conjunct = {
  exp : expr;
  valid_conjunct : validity option; (*non-rhs conjuncts don't have a validity*)
  counter_example_conjunct : Counterexamples.example list option;
  in_inductive_core : bool ref;
}
and function_validity_information = {
  fn : fnDecl;
  termination_result : termination_result option;
  correctness_result : correctness_result;
  overall_validity : validity;
}

let location_of_vc_conjunct_list_list cons = 
  let cur_loc = ref (gdl()) in
  let process_vc_conjunct_list cons =    
    let process_vc_conjunct con = 
      cur_loc := location_union !cur_loc (location_of_expr con.exp)
    in
      List.iter process_vc_conjunct cons
  in
    List.iter process_vc_conjunct_list cons;
    !cur_loc
    
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
type 'a thread_response =
  | Normal of 'a
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
  Config.print ("VC cache is evicting " ^ oldest_member);
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
let verify_vc_expr (vc_with_preds, (vc_cache, cache_lock), program) =
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
        Config.print ("Loaded from cache: " ^ unique_vc_str ^ " is " ^ (string_of_validity (fst result)));
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















let rec overall_validity_of_function_validity_information_list function_validity_information_list = 
  let function_is_invalid vc = 
    vc.overall_validity = Invalid
  in
  let function_is_unknown vc = 
    vc.overall_validity = Unknown
  in
    if List.exists function_is_invalid function_validity_information_list then Invalid
    else if List.exists function_is_unknown function_validity_information_list then Unknown
    else Valid

let rec overall_validity_of_vc_detailed_list vc_detailed_list = 
  let vc_is_invalid vc = 
    vc.valid = Invalid
  in
  let vc_is_unknown vc = 
    vc.valid = Unknown
  in
    if List.exists vc_is_invalid vc_detailed_list then Invalid
    else if List.exists vc_is_unknown vc_detailed_list then Unknown
    else Valid

and vc_detailed_of_vc_detailed_temp vc_detailed_temp = 
  let elem_is_invalid elem = 
    (Utils.elem_from_opt elem.valid_conjunct) = Invalid
  in
  let elem_is_unknown elem = 
    (Utils.elem_from_opt elem.valid_conjunct) = Unknown
  in
  let rhs = List.nth vc_detailed_temp.vc_temp ((List.length vc_detailed_temp.vc_temp)-1) in
  let valid = 
    if List.exists (elem_is_invalid) rhs then
      Invalid
    else if List.exists (elem_is_unknown) rhs then
      Unknown
    else
      Valid
  in
  let counter_example = 
    if valid=Invalid then
      let invalid_elem = List.find elem_is_invalid rhs in
        invalid_elem.counter_example_conjunct
    else
      None
  in {vc = vc_detailed_temp.vc_temp;valid=valid;counter_example=counter_example; bp = vc_detailed_temp.bp_temp;}


and expr_of_vc_detailed_temp_just_core vc_detailed_temp = 
  expr_of_implies_list vc_detailed_temp.vc_temp true

and expr_of_vc_detailed vc_detailed = 
  expr_of_implies_list vc_detailed.vc false

and expr_of_implies_list implies_list core_only = 
  match List.length implies_list with
      0 -> assert(false)
    | 1 -> expr_of_conjunct_list (List.hd implies_list) core_only
    | _ -> Implies(gdl(), expr_of_conjunct_list (List.hd implies_list) core_only, (expr_of_implies_list (List.tl implies_list) core_only))
        
and expr_of_conjunct_list conjunct_list core_only = 
  match List.length conjunct_list with
      0 -> assert(false)
    | 1 ->
        begin
          let conjunct = (List.hd conjunct_list) in
            if core_only || conjunct.in_inductive_core.contents then
              conjunct.exp
            else
              Constant(gdl(), ConstBool(gdl(), true))
        end
    | _ -> And(gdl(), (List.hd conjunct_list).exp, expr_of_conjunct_list (List.tl conjunct_list) core_only)


and vc_temp_of_expr expr func bp = 
  let rec get_implies_list expr = 
    let rec get_conjunct_list expr = 
      match expr with 
          And(loc,e1,e2) -> get_conjunct_list e1 @ get_conjunct_list e2
        | _ -> [{exp=expr;valid_conjunct=None;in_inductive_core=ref true;counter_example_conjunct=None;}]
    in
      match expr with
          Implies(loc,lhs,rhs) -> [get_conjunct_list lhs] @ get_implies_list rhs
        | _ -> [get_conjunct_list expr]
  in
    {vc_temp = get_implies_list expr; func_temp = func; bp_temp = bp}
      

let verify_vc (vc,vc_cache_and_lock,program_ast,set_valid,set_in_core) =
  try
    if not set_valid && not set_in_core then assert(false);
    let threads_of_vc vc =
      let expr_of_vc_advanced_replace_rhs vc new_rhs = 
        let rec get_new expr  = 
          match expr with
              Implies(loc,e1,e2) -> Implies(loc,e1,get_new e2)
            | _ -> new_rhs
        in
          get_new (expr_of_vc_detailed_temp_just_core vc)
      in
      let thread_of_conjunct conj = 
        let vc_expr = expr_of_vc_advanced_replace_rhs vc conj.exp in
          (conj, Background.create verify_vc_expr (vc_expr, vc_cache_and_lock, program_ast))
      in
      let rhs = (List.nth vc.vc_temp ((List.length vc.vc_temp) - 1)) in
        List.map thread_of_conjunct rhs
    in    
    let make_new_vc_from_threads threads = 
      let new_conjunct_of_thread conj_and_thread = 
        let old_conj = fst conj_and_thread in
        let thread = snd conj_and_thread in
        let vc_result =
          let vc_result_tmp = Background.get_result thread in
	    match vc_result_tmp with
	      | Normal (v, c) -> (v, c)
	      | Exceptional (e) -> raise e
        in
        let valid_conjunct = 
          if set_valid then
            Some(fst vc_result)
          else
            old_conj.valid_conjunct
        in
        let counter_example_conjunct =
          if set_valid then
            snd vc_result
          else
            old_conj.counter_example_conjunct
        in
          begin
            if fst vc_result != Valid && set_in_core then
              old_conj.in_inductive_core := false
          end;
          {exp=old_conj.exp; valid_conjunct=valid_conjunct; counter_example_conjunct=counter_example_conjunct; in_inductive_core=old_conj.in_inductive_core;}
      in
      let new_rhs = List.map new_conjunct_of_thread threads in
      let new_imples_conjs = (List.rev (List.tl (List.rev vc.vc_temp))) (*i.e. everything but the first elem*) @ [new_rhs] in
        {vc_temp = new_imples_conjs;func_temp = vc.func_temp;bp_temp=vc.bp_temp}          
    in    
    let threads = threads_of_vc vc in
    let new_vc = make_new_vc_from_threads threads in
      Normal(new_vc)
  with ex -> Exceptional (ex) ;;


        
(*Compares exprs based on their locations. So two exprs with the same locations
  are deemed the same.*)
module Expr_map = Map.Make (struct
                              type t = expr
                              let compare exp1 exp2 =
                                let loc1 = (location_of_expr exp1).loc_start in
                                let loc2 = (location_of_expr exp2).loc_start in
                                  compare_locs loc1 loc2
                            end)

(*Compares functions based on their locations. So two functions with the same location
  are deemed the same.*)
module Fn_map = Map.Make (struct
                              type t = fnDecl
                              let compare fun1 fun2 =
                                  compare_locs fun1.location_fd.loc_start fun2.location_fd.loc_start
                            end)

let verify_program_correctness program_info program_ast vc_cache_and_lock =  
  (*We begin by making a list of all the VCs and dividing them up into their conjuncts*)
  let detailed_vcs = 
    let already_assigned = ref Expr_map.empty in
    let vcs = ref [] in
    let add_vcs_in_func (fn, norm_bps, term_bps, nvcs) =
      let add_vc_in_bp (bp,expr) = 
        let rec get_conjunct_list expr = 
          match expr with 
              And(loc,e1,e2) -> get_conjunct_list e1 @ get_conjunct_list e2
            | _ ->
                begin
                  let is_inductive_ref = 
                    try
                      Expr_map.find expr !already_assigned
                    with
                        ex ->
                          begin
                            let new_is_inductive_ref = ref true in
                              already_assigned := Expr_map.add expr new_is_inductive_ref !already_assigned;
                              new_is_inductive_ref
                          end
                  in
                    [{exp=expr;valid_conjunct=None;in_inductive_core=is_inductive_ref;counter_example_conjunct=None;}]
                end
        in
        let rec get_implies_list expr = 
          match expr with
              Implies(loc,lhs,rhs) -> [get_conjunct_list lhs] @ get_implies_list rhs
            | _ -> [get_conjunct_list expr]
                
        in vcs := !vcs @ [{vc_temp=(get_implies_list expr);func_temp = fn; bp_temp=Some(bp)}]
      in
        List.iter add_vc_in_bp norm_bps
    in
      List.iter add_vcs_in_func program_info;
      !vcs
  in
    (*Now we'll run Aaron's algorithm on those VCs*)
  let iterate_aarons_algorithm_once vcs is_first_run = 
    let threads_of_vcs vcs = 
      let threads_of_vc vc =
        let expr_of_vc_advanced_replace_rhs vc new_rhs = 
          let rec get_new expr  = 
            match expr with
                Implies(loc,e1,e2) -> Implies(loc,e1,get_new e2)
              | _ -> new_rhs
          in
            get_new (expr_of_vc_detailed_temp_just_core vc)
        in
        let thread_of_conjunct conj = 
          let vc_expr = expr_of_vc_advanced_replace_rhs vc conj.exp in
            (conj, Background.create verify_vc_expr (vc_expr, vc_cache_and_lock, program_ast))
        in
        let rhs = (List.nth vc.vc_temp ((List.length vc.vc_temp) - 1)) in
          (vc,List.map thread_of_conjunct rhs)
      in    
        List.map threads_of_vc vcs
    in
    let make_new_vcs_from_threads threads = 
      let make_new_vc_from_threads vc_and_threads = 
        let old_vc = fst vc_and_threads in
        let threads = snd vc_and_threads in
        let new_conjunct_of_thread conj_and_thread = 
          let old_conj = fst conj_and_thread in
          let thread = snd conj_and_thread in
          let vc_result =
            let vc_result_tmp = Background.get_result thread in
	      match vc_result_tmp with
	        | Normal (v, c) -> (v, c)
	        | Exceptional (e) -> raise e
          in
          let valid_conjunct = 
            if is_first_run then
              Some(fst vc_result)
            else
              old_conj.valid_conjunct
          in
          let counter_example_conjunct =
            if is_first_run then
              snd vc_result
            else
              old_conj.counter_example_conjunct
          in
            begin
              if fst vc_result != Valid then
                old_conj.in_inductive_core := false
            end;
            {exp=old_conj.exp; valid_conjunct=valid_conjunct; counter_example_conjunct=counter_example_conjunct; in_inductive_core=old_conj.in_inductive_core;}
        in
        let new_rhs = List.map new_conjunct_of_thread threads in
        let new_imples_conjs = (List.rev (List.tl (List.rev old_vc.vc_temp))) (*i.e. everything but the first elem*) @ [new_rhs] in
          {vc_temp = new_imples_conjs;func_temp = old_vc.func_temp;bp_temp = old_vc.bp_temp}          
      in
        List.map make_new_vc_from_threads threads
    in
    let threads = threads_of_vcs vcs in
      make_new_vcs_from_threads threads
  in
  let iterate_aarons_algorithm_until_convergence vcs = 
    let rec iterate_aarons_algorithm_until_convergence_rec vcs = 
      let grab_absolute_ref_pairs_from_vcs vcs = 
        let grab_absolute_ref_pairs_from_vc vc = 
          let grab_absolute_ref_pairs_from_conjunct_list conj_list = 
            let grab_absolute_ref_pairs_from_conjunct conj = 
              (conj.in_inductive_core.contents,conj.in_inductive_core)
            in
              List.map grab_absolute_ref_pairs_from_conjunct conj_list
          in
            List.flatten (List.map grab_absolute_ref_pairs_from_conjunct_list vc.vc_temp)
        in
          List.flatten (List.map grab_absolute_ref_pairs_from_vc vcs)
      in
      let backup_bools = grab_absolute_ref_pairs_from_vcs vcs in
      let new_vcs = iterate_aarons_algorithm_once vcs false in
      let change_has_occured = 
        let is_different (a,b) =
          a!=b.contents
        in
          List.exists is_different backup_bools
      in
        if change_has_occured then
          iterate_aarons_algorithm_until_convergence_rec new_vcs
        else
          new_vcs
    in
    let vcs_after_first_round = iterate_aarons_algorithm_once vcs true in
      iterate_aarons_algorithm_until_convergence_rec vcs_after_first_round
  in
  let group_vcs_by_function_and_make_non_temp vcs = 
    let func_map = ref Fn_map.empty in
    let grouped_vcs = ref [] in
    let add_vc_to_map vc = 
      begin
        if not (Fn_map.mem vc.func_temp !func_map) then 
          func_map := Fn_map.add vc.func_temp [] !func_map
      end;
      let old_list = Fn_map.find vc.func_temp !func_map in
        func_map := Fn_map.add vc.func_temp (old_list @ [vc_detailed_of_vc_detailed_temp vc]) !func_map
    in
    let deal_with_key_value key value = 
      grouped_vcs := !grouped_vcs @ [(key,value)]
    in
      List.iter add_vc_to_map vcs;
      Fn_map.iter deal_with_key_value !func_map;
      !grouped_vcs
  in
  let new_vcs = iterate_aarons_algorithm_until_convergence detailed_vcs in
  let grouped_non_temp_vcs = group_vcs_by_function_and_make_non_temp new_vcs in
  let fn_correctness_result_pair_of_fn_vc_list_pair (fn, vcs) = 
    let overall_validity_c = overall_validity_of_vc_detailed_list vcs in
      (fn, {overall_validity_c = overall_validity_c; vcs = vcs})
  in
    List.map fn_correctness_result_pair_of_fn_vc_list_pair grouped_non_temp_vcs
      


let rec verify_program_termination program_info program_ast vc_cache_and_lock =
  (* First, build an intermediate structure that
     contains a Background process for each VC.
     That is, we start all the VC requests. *)
  let intermediate_info =
    (* Makes a data structure where we replace each VC expr with VC expr * VC background thread. *)
    let intermediate_fn (fn, norm_bps, term_bps, nvcs) =
      let get_vc_thread vc = Background.create verify_vc (vc,vc_cache_and_lock,program_ast,true,false) in
      let intermediate_basic_path (path,vc) =
        let vc_temp = vc_temp_of_expr vc fn (Some(path)) in
        let vc_thread = get_vc_thread vc_temp in
          vc_thread
      in
      let intermediate_nonnegative_vc nvc =
        let vc_temp = vc_temp_of_expr nvc fn None in
        let vc_thread = get_vc_thread vc_temp in
          vc_thread
      in
      let term_path = List.map intermediate_basic_path term_bps in
      let nvc = List.map intermediate_nonnegative_vc nvcs in
        (fn, term_path, nvc)
    in
      List.map intermediate_fn program_info
  in

  (* Verify a function.
     Takes in a tuple of its name, its
     termination basic paths, and the non-negative VCs.
     We first call the verify_* fns to start all the background VC requests.
     We then get all of the results and get the overall validities. *)
  let rec verify_function (fn, term_paths, nvcs) =
    (* Gets the result of a thread and either returns it or
       raises the exception that the thread raised. *)
    let get_vc_result vc_thread = 
      let vc_result = Background.get_result vc_thread in
	match vc_result with
	  | Normal (v) -> (v)
	  | Exceptional (e) -> raise e
    in
    let verify_term_path vc_thread =
      vc_detailed_of_vc_detailed_temp (get_vc_result vc_thread)
    in
    let verify_nonnegative_vc vc_thread =
      vc_detailed_of_vc_detailed_temp (get_vc_result vc_thread)
    in
    let verified_term_paths = List.map verify_term_path term_paths in
    let validity_of_term_path term_path = term_path.valid in
    let overall_term_path_status = overall_validity_status verified_term_paths validity_of_term_path in
      
    let termination_result =
      if (List.length term_paths > 0) then
        begin
(*
          let detailed_vc_of_verfied_term_path (bp,vc,valid,counter_example) =
            {vc = [[{exp=vc;valid_conjunct=Some(valid);counter_example_conjunct=counter_example;in_inductive_core=ref true;}]];
             bp = Some(bp);
             valid = valid;
             counter_example = counter_example;
            }
          in
            
          let detailed_vc_of_non_negative_vc (vc,valid,counter_example) =
            {vc = [[{exp=vc;valid_conjunct=Some(valid);counter_example_conjunct=counter_example;in_inductive_core=ref true}]];
             bp = None;
             valid = valid;
             counter_example = counter_example;
            }
          in
*)            
	  let verified_nvcs = List.map verify_nonnegative_vc nvcs in
	  let validity_of_nvc nvc = nvc.valid in
	  let overall_nvc_status = overall_validity_status verified_nvcs validity_of_nvc in
	  let overall_validity = overall_validity [overall_term_path_status; overall_nvc_status] in

(*          let decreasing_paths = List.map detailed_vc_of_verfied_term_path verified_term_paths in
            let nonnegative_vcs = List.map detailed_vc_of_non_negative_vc verified_nvcs in
*)            
          let termination_info = {
            overall_validity_t = overall_validity;
            decreasing_paths_validity = overall_term_path_status;
            nonnegative_vcs_validity = overall_nvc_status;
            decreasing_paths = verified_term_paths;
            nonnegative_vcs = verified_nvcs;
          } 
          in
	    Some (termination_info)
        end
      else
	None
    in
    (fn, termination_result)
  in
  List.map verify_function intermediate_info



(*TODO-J: finish this*)
(*let create_termination_result ov paths nvcs =
  { overall_validity_t = ov; decreasing_paths = paths; nonnegative_vcs = nvcs; decreasing_paths_validity = Unknown; nonnegative_vcs_validity = Unknown;  } ;;
*)

and verify_program program_info program_ast vc_cache_and_lock =
  let correctness_results = verify_program_correctness program_info program_ast vc_cache_and_lock in
  let termination_results = verify_program_termination program_info program_ast vc_cache_and_lock in
  let func_map = ref Fn_map.empty in
  let add_func_to_map (func,_,_,_) = 
    func_map := Fn_map.add func (ref None, ref None) !func_map
  in
  let put_correctness_result_in_map correctness_result =
    let (c, t) = Fn_map.find (fst correctness_result) !func_map in
      c := Some(snd correctness_result)
  in
  let put_termination_result_in_map termination_result =
    let (c, t) = Fn_map.find (fst termination_result) !func_map in
      t := Some(snd termination_result)
  in
  let list_of_map m = 
    let l = ref [] in
    let process_key_value fn (c,t) = 
      let correctness_result = elem_from_opt(!c) in
      let termination_result = elem_from_opt(!t) in
      let overall_validity =
        match termination_result with
            Some(termination_result) ->
              begin
                if correctness_result.overall_validity_c = Invalid or termination_result.overall_validity_t = Invalid then
                  Invalid
                else if correctness_result.overall_validity_c = Unknown or termination_result.overall_validity_t = Unknown then
                  Unknown
                else Valid
              end
          | None -> correctness_result.overall_validity_c
      in
      let list_elem = {fn=fn;correctness_result=correctness_result;termination_result=termination_result;overall_validity=overall_validity;} in
        l := !l @ [list_elem]
    in
      Fn_map.iter process_key_value m;
      !l
  in
    (*Step 1: Add all functions to the map without any other information*)
    List.iter add_func_to_map program_info;
    (*Step 2: Populate the elements in the map with the correctness results*)
    List.iter put_correctness_result_in_map correctness_results;
    (*Step 3: Populate the elements in the map with the termination results*)
    List.iter put_termination_result_in_map termination_results;
    list_of_map !func_map


