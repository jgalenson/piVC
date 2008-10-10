(* piVC *)

open Parser
open Ast
open Semantic_checking
open Background
open Utils
open Lexing

exception Dp_server_exception of string ;;
exception Malformatted_DP_Server_Address;;
exception Invalid_DP_Response of string ;;

type verification_mode = Set_validity | Set_in_core

type validity = Valid | Invalid | Unknown
        
and termination_result = {
  overall_validity_t : validity;
  decreasing_paths_validity : validity;
  nonnegative_vcs_validity : validity;
  decreasing_paths : verification_atom list;
  nonnegative_vcs : verification_atom list;
}
and correctness_result = {
  overall_validity_c : validity;
  vcs : verification_atom list;
} 
and atom_info =
  | BP of Basic_paths.basic_path
  | RankingFunc of Ast.rankingAnnotation
and verification_atom = {
  vc : vc_conjunct list list;
  info : atom_info;  
  valid : validity;
  counter_example : Smt_solver.Counterexample.example list option;
}
and verification_atom_temp = {
  func_temp: fnDecl;
  vc_temp : vc_conjunct list list;
  info_temp : atom_info;
}
and vc_conjunct = {
  exp : expr;
  valid_conjunct : validity option; (*non-rhs conjuncts don't have a validity*)
  counter_example_conjunct : Smt_solver.Counterexample.example list option;
  in_inductive_core : (bool ref) option;
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

let name_of_verification_atom va =
  match va.info with
      BP(bp) -> Basic_paths.name_of_basic_path bp
    | RankingFunc(rf) -> Ast.string_of_ranking_annotation rf

let location_of_verification_atom va = 
  match va.info with
      BP(bp) -> location_of_vc_conjunct_list_list va.vc
    | RankingFunc(rf) -> rf.location_ra
        
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
              Expr_utils.sub_idents_in_expr_while_preserving_original_location pred_decl.expr replacement_pairs
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
      | NewArray (loc, t, e) -> assert(false);
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
  (*let begin_time = Unix.time () in*)
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
  
      (*Utils.debug_print_time_diff begin_time (Unix.time ()) "before vc final: ";*)
          
          (*let before_vc_time = Unix.time () -. begin_time in*)
          
          (*let before_final_vc_time = Unix.time () in*)
          
	  (* We negate it since F is valid iff not F is unsat. *)
          let final_vc = Expr_utils.remove_quantification_from_vc_with_array_dp (Not (get_dummy_location (), (Verification_conditions.add_array_length_greater_than_0_to_expr vc))) in
            
          (*let after_vc_time = Unix.time () -. begin_time in*)
          (*let vc_time = Unix.time () -. before_final_vc_time in*)
            
          (*print_endline ("before vc time: " ^ (string_of_float before_vc_time) ^ "\nafter vc time : " ^ (string_of_float after_vc_time) ^ "\nvc time : " ^ (string_of_float vc_time) ^ "\n\n\n\n");*)
            

            
     (*Note from Jason: do not remove any commented-out code from this file. I might need it for later debugging.*)
(*          let negated_vc = (Not (get_dummy_location (), vc)) in
          let negated_vc_nnf = Expr_utils.nnf (Not (get_dummy_location (), vc)) in
          let vc_no_quants = Expr_utils.remove_quantification_from_vc_with_array_dp vc in
          let vc_no_quants_array_lengths_geq_0 = (*Verification_conditions.add_array_length_greater_than_0_to_expr*) vc_no_quants in
          let negated_vc_no_quants_array_lengths_geq_0 = (Not (get_dummy_location (), vc_no_quants_array_lengths_geq_0)) in
            
            print_endline ("*********************************");
            print_endline ("VC in NNF is: \n" ^ string_of_expr negated_vc_nnf);
            print_endline ("Index set is as follows:");
            let print_expr exp = print_endline (Expr_utils.guaranteed_unique_string_of_expr exp) in
              List.iter print_expr (Expr_utils.get_index_set negated_vc_nnf);
              print_string ("Gave the following VC to the SMT solver: \n" ^ string_of_expr negated_vc_no_quants_array_lengths_geq_0 ^ "\n");
              (*print_string ("And got a response of: " ^ response ^ "\n");*)
              print_endline ("*********************************");
*)
              
            
            
          let (vc, rev_var_names) = Smt_solver.transform_input final_vc in
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
		    let counterexample = Smt_solver.parse_counterexample counterexample_str rev_var_names in
	              (Invalid, Some (counterexample))
	          end
	        else if (response = "error") then
	          begin
	            let error_msg = Net_utils.get_input inchan in
	              raise (Dp_server_exception error_msg)
	          end
		else if response = "non-linear" then
		  (Unknown, None)
                else
		  raise (Invalid_DP_Response response)
              in
                Mutex.lock cache_lock;
                add_to_cache vc_cache unique_vc_str result;
                Mutex.unlock cache_lock;
                Normal (fst result, snd result)
        end
  with
      Expr_utils.OutsideFragment -> Normal(Unknown,None)
    | ex -> Exceptional (ex)

let overall_validity_status list_of_things extraction_func = 
  let is_validity extraction_func test actual = (test==(extraction_func actual)) in
    if (List.for_all (is_validity extraction_func Valid) list_of_things) then Valid
    else if (List.exists (is_validity extraction_func Invalid) list_of_things) then Invalid
    else Unknown
;;




(* Gets all the info we need from a program.
   That is, for each method, its basic paths and VCs: (path_node list * expr).list
   Returns (fn * (Normal Basic Path * VC) list * (Term basic path * VC list) * non-negative VC list) list. *)
let get_all_info program options =
  (* Returns a list of pairs of fnName and its basic path.
     Returns (fn * (basic_path list * (norm_basic_path list, term_basic_path list))) list. *)
  let get_basic_paths program =
    let get_decl_paths_if_appropriate decl = 
      match decl with
          VarDecl (loc, vd) -> None
        | Predicate (loc, p) -> None
	| FnDecl (loc, fd) -> (Some (fd, Basic_paths.generate_paths_for_func fd program options.generate_runtime_assertions))
        | ClassDecl (loc, cd) -> None (*change this if you ever allow classes to have methods*)
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
  let get_vcs (fndecl, (normal_paths, termination_paths)) =
    let norm_vcs = List.map (fun path -> (path, Verification_conditions.get_vc path program)) normal_paths in
    let term_vcs = List.map (fun path -> (path, Verification_conditions.get_vc path program)) termination_paths in
    let nonneg_vcs = Termination.get_nonnegativity_vcs fndecl program in
    (fndecl, norm_vcs, term_vcs, nonneg_vcs)
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
  (if (List.length vc_detailed_temp.vc_temp)==0 then assert(false));
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
  in {vc = vc_detailed_temp.vc_temp;valid=valid;counter_example=counter_example; info = vc_detailed_temp.info_temp;}


and expr_of_vc_detailed_temp_just_core vc_detailed_temp = 
  expr_of_implies_list vc_detailed_temp.vc_temp true

and expr_of_vc_detailed_temp vc_detailed_temp = 
  expr_of_implies_list vc_detailed_temp.vc_temp false

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
            if (not core_only) || (elem_from_opt conjunct.in_inductive_core).contents then
              conjunct.exp
            else
              Constant(gdl(), ConstBool(gdl(), true))
        end
    | _ -> And(gdl(), expr_of_conjunct_list [(List.hd conjunct_list)] core_only, expr_of_conjunct_list (List.tl conjunct_list) core_only)


and vc_temp_of_expr_with_inductive_core expr func bp map_for_inductive_core = 
  vc_temp_of_expr_helper expr func bp (Some(map_for_inductive_core))
    
and vc_temp_of_expr expr func bp = 
  vc_temp_of_expr_helper expr func bp None
    
and vc_temp_of_expr_helper expr func info map_for_inductive_core = 
  let rec get_implies_list expr =
    let rec get_conjunct_list expr = 
      match expr with 
          And(loc,e1,e2) -> get_conjunct_list e1 @ get_conjunct_list e2
        | _ ->
            begin
              let in_inductive_core = 
                match map_for_inductive_core with
                    Some(m) ->
                      begin
                        match is_dummy_location (location_of_expr expr) with
                            true -> Some(ref true)
                          | false -> 
                              begin
                                try
                                  Some(Ast_utils.Expr_map.find expr m.contents)
                                with
                                    Not_found ->
                                      begin
                                        let new_is_inductive_ref = ref true in
                                          m := Ast_utils.Expr_map.add expr new_is_inductive_ref !m;
                                          Some(new_is_inductive_ref)
                                      end
                              end
                      end
                  | None -> None
              in              
                [{exp=expr;valid_conjunct=None;in_inductive_core=in_inductive_core;counter_example_conjunct=None}]
            end
    in
      match expr with
          Implies(loc,lhs,rhs) -> [get_conjunct_list lhs] @ get_implies_list rhs
        | _ -> [get_conjunct_list expr]
  in
    {vc_temp = get_implies_list expr; func_temp = func; info_temp = info}
      


(*        let rec get_conjunct_list expr = 
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
                    [{exp=expr;valid_conjunct=None;in_inductive_core=Some(is_inductive_ref);counter_example_conjunct=None;}]
                end
        in
        let rec get_implies_list expr = 
          match expr with
              Implies(loc,lhs,rhs) -> [get_conjunct_list lhs] @ get_implies_list rhs
            | _ -> [get_conjunct_list expr]
*)  

let verify_vc (vc,vc_cache_and_lock,program_ast,mode) =
  try
    (if (List.length vc.vc_temp)==0 then assert(false));
    let threads_of_vc vc =
      let expr_of_vc_advanced_replace_rhs vc new_rhs = 
        let rec get_new expr  = 
          match expr with
              Implies(loc,e1,e2) -> Implies(loc,e1,get_new e2)
            | _ -> new_rhs
        in
          match mode with
              Set_in_core -> get_new (expr_of_vc_detailed_temp_just_core vc)
            | Set_validity -> get_new (expr_of_vc_detailed_temp vc)
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
        let (valid_conjunct, counter_example_conjunct) = 
          if mode=Set_validity then
            (Some(fst vc_result),snd vc_result)
          else
            (old_conj.valid_conjunct, old_conj.counter_example_conjunct)
        in
          begin
            if fst vc_result != Valid && mode=Set_in_core then
              elem_from_opt (old_conj.in_inductive_core) := false
          end;
          {exp=old_conj.exp; valid_conjunct=valid_conjunct; counter_example_conjunct=counter_example_conjunct; in_inductive_core=old_conj.in_inductive_core;}
      in
      let new_rhs = List.map new_conjunct_of_thread threads in
      let new_imples_conjs = (List.rev (List.tl (List.rev vc.vc_temp))) (*i.e. everything but the first elem*) @ [new_rhs] in
        {vc_temp = new_imples_conjs;func_temp = vc.func_temp;info_temp=vc.info_temp}          
    in    
    let threads = threads_of_vc vc in
    let new_vc = make_new_vc_from_threads threads in
      Normal(new_vc)
  with ex -> Exceptional (ex) ;;



let verify_vcs vcs vc_cache_and_lock program_ast mode = 
  let threads_of_vcs vcs = 
    let thread_of_vc vc =
      Background.create verify_vc (vc,vc_cache_and_lock,program_ast,mode);
    in
      List.rev_map thread_of_vc vcs
  in
  let make_new_vcs_from_threads threads = 
    let make_new_vc_from_thread thread = 
      match (Background.get_result thread) with
	| Normal (v) -> v
	| Exceptional (e) -> raise e
    in
      List.rev_map make_new_vc_from_thread threads
  in
(*  let process_one_atom atom =
    let threads = threads_of_vcs [atom] in
      List.hd (make_new_vcs_from_threads threads)
  in*)
  let threads = threads_of_vcs vcs in
        make_new_vcs_from_threads threads
    (*List.map process_one_atom vcs*)



let verify_program_correctness program_info program_ast vc_cache_and_lock already_assigned options =  
  (*We begin by making a list of all the VCs and dividing them up into their conjuncts*)
  let detailed_vcs = 
    let vcs = ref [] in
    let add_vcs_in_func (fn, norm_bps, term_bps, nvcs) =
      let add_vc_in_bp (bp,expr) = 
        let new_vc = 
          if options.find_inductive_core then
            vc_temp_of_expr_with_inductive_core expr fn (BP(bp)) already_assigned
          else
            vc_temp_of_expr expr fn (BP(bp))
        in
          vcs := !vcs @ [new_vc]
      in
        List.iter add_vc_in_bp norm_bps
    in
      List.iter add_vcs_in_func program_info;
      !vcs
  in
  let iterate_aarons_algorithm_until_convergence vcs = 
    let rec iterate_aarons_algorithm_until_convergence_rounds_2_onwards vcs = 
      let grab_absolute_ref_pairs_from_vcs vcs = 
        let grab_absolute_ref_pairs_from_vc vc = 
          let grab_absolute_ref_pairs_from_conjunct_list conj_list = 
            let grab_absolute_ref_pairs_from_conjunct conj = 
              ((elem_from_opt (conj.in_inductive_core)).contents, elem_from_opt (conj.in_inductive_core))
            in
              List.map grab_absolute_ref_pairs_from_conjunct conj_list
          in
            List.flatten (List.map grab_absolute_ref_pairs_from_conjunct_list vc.vc_temp)
        in
          List.flatten (List.map grab_absolute_ref_pairs_from_vc vcs)
      in
      let backup_bools = grab_absolute_ref_pairs_from_vcs vcs in
      let new_vcs = verify_vcs vcs vc_cache_and_lock program_ast Set_in_core in
      let change_has_occured = 
        let is_different (a,b) =
          a!=b.contents
        in
          List.exists is_different backup_bools
      in
        if change_has_occured then
          iterate_aarons_algorithm_until_convergence_rounds_2_onwards new_vcs
        else
          new_vcs
    in
    let vcs_with_validity = verify_vcs vcs vc_cache_and_lock program_ast Set_validity in
      iterate_aarons_algorithm_until_convergence_rounds_2_onwards vcs_with_validity
  in
  let group_vcs_by_function_and_make_non_temp vcs = 
    let func_map = ref Ast_utils.Fn_map.empty in
    let grouped_vcs = ref [] in
    let add_vc_to_map vc = 
      begin
        if not (Ast_utils.Fn_map.mem vc.func_temp !func_map) then 
          func_map := Ast_utils.Fn_map.add vc.func_temp [] !func_map
      end;
      let old_list = Ast_utils.Fn_map.find vc.func_temp !func_map in
        func_map := Ast_utils.Fn_map.add vc.func_temp (old_list @ [vc_detailed_of_vc_detailed_temp vc]) !func_map
    in
    let deal_with_key_value key value = 
      grouped_vcs := !grouped_vcs @ [(key,value)]
    in
      List.iter add_vc_to_map vcs;
      Ast_utils.Fn_map.iter deal_with_key_value !func_map;
      !grouped_vcs
  in
  let new_vcs = 
    if options.find_inductive_core then iterate_aarons_algorithm_until_convergence detailed_vcs
    else verify_vcs detailed_vcs vc_cache_and_lock program_ast Set_validity
  in
  let grouped_non_temp_vcs = group_vcs_by_function_and_make_non_temp new_vcs in
  let fn_correctness_result_pair_of_fn_vc_list_pair (fn, vcs) = 
    let overall_validity_c = overall_validity_of_vc_detailed_list vcs in
      (fn, {overall_validity_c = overall_validity_c; vcs = vcs})
  in
    List.map fn_correctness_result_pair_of_fn_vc_list_pair grouped_non_temp_vcs
      


let rec verify_program_termination program_info program_ast vc_cache_and_lock already_assigned options =
  (* First, build an intermediate structure that
     contains a Background process for each VC.
     That is, we start all the VC requests. *)


  let get_vc_with_validity_and_core_set vc = 
    let vc_with_validity = verify_vc (vc,vc_cache_and_lock,program_ast,Set_validity) in
      match vc_with_validity with
          Normal(vc) -> 
            begin
              if options.find_inductive_core then
                let vc_with_validity_and_core_set = verify_vc (vc,vc_cache_and_lock,program_ast,Set_in_core) in
                  match vc_with_validity_and_core_set with
                      Normal(vc) -> Normal(vc)
                    | Exceptional(e) -> Exceptional(e)
              else Normal(vc)
            end
        | Exceptional(e) -> Exceptional(e)
  in
  let intermediate_info =
    (* Makes a data structure where we replace each VC expr with VC expr * VC background thread. *)
    let intermediate_fn (fn, norm_bps, term_bps, nvcs) =
      let get_vc_thread vc = Background.create get_vc_with_validity_and_core_set vc (*verify_vc (vc,vc_cache_and_lock,program_ast,Set_validity)*) in
      let intermediate_basic_path (path,vc) =
        let vc_temp = vc_temp_of_expr_with_inductive_core vc fn (BP(path)) already_assigned in
        let vc_thread = get_vc_thread vc_temp in
          vc_thread
      in
      let intermediate_nonnegative_vc (rankingFunc, vc) =
        let vc_temp = vc_temp_of_expr_with_inductive_core vc fn (RankingFunc(rankingFunc)) already_assigned in
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
	  let verified_nvcs = List.map verify_nonnegative_vc nvcs in
	  let validity_of_nvc nvc = nvc.valid in
	  let overall_nvc_status = overall_validity_status verified_nvcs validity_of_nvc in
	  let overall_validity = overall_validity [overall_term_path_status; overall_nvc_status] in          
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



and verify_program program_info program_ast vc_cache_and_lock options =
  let already_assigned = ref Ast_utils.Expr_map.empty in
  let correctness_results = verify_program_correctness program_info program_ast vc_cache_and_lock already_assigned options in
  let termination_results = verify_program_termination program_info program_ast vc_cache_and_lock already_assigned options in
  let func_map = ref Ast_utils.Fn_map.empty in
  let add_func_to_map (func,_,_,_) = 
    func_map := Ast_utils.Fn_map.add func (ref None, ref None) !func_map
  in
  let put_correctness_result_in_map correctness_result =
    let (c, t) = Ast_utils.Fn_map.find (fst correctness_result) !func_map in
      c := Some(snd correctness_result)
  in
  let put_termination_result_in_map termination_result =
    let (c, t) = Ast_utils.Fn_map.find (fst termination_result) !func_map in
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
      Ast_utils.Fn_map.iter process_key_value m;
      !l
  in
    (*Step 1: Add all functions to the map without any other information*)
    List.iter add_func_to_map program_info;
    (*Step 2: Populate the elements in the map with the correctness results*)
    List.iter put_correctness_result_in_map correctness_results;
    (*Step 3: Populate the elements in the map with the termination results*)
    List.iter put_termination_result_in_map termination_results;
    list_of_map !func_map



let contains_unknown_vc function_validity_information_list =
  let function_contains_unknown function_validity_information = 
    let atom_contains_unknown atom =
      atom.valid = Unknown
    in
      List.exists atom_contains_unknown function_validity_information.correctness_result.vcs ||
        match function_validity_information.termination_result with
            None -> false
          | Some(term) ->
              List.exists atom_contains_unknown term.decreasing_paths ||
                List.exists atom_contains_unknown term.nonnegative_vcs
  in
    List.exists function_contains_unknown function_validity_information_list

let inductive_core_good_enough function_validity_information_list =
  let important_vc_implies_all_rhs_conjuncts_in_inductive_core vc = 
    let rhs_all_in_core vc = 
      let rhs = List.nth vc (List.length vc - 1) in
      let elem_is_in_core elem = 
        (elem_from_opt (elem.in_inductive_core)).contents
      in
        List.for_all elem_is_in_core rhs
    in
      match vc.info with
          BP(bp) ->
            begin
              match bp with
                  Basic_paths.NormalPath(p,e) ->
                    begin
                      match e with
                          Basic_paths.PostConditionEnding -> rhs_all_in_core vc.vc
                        | Basic_paths.AssertEnding -> rhs_all_in_core vc.vc
                        | Basic_paths.AnnotationEnding -> true
                        | Basic_paths.CallEnding -> true
                    end
                | Basic_paths.RuntimeAssertPath(p) -> rhs_all_in_core vc.vc
                | Basic_paths.TerminationPath(p) -> rhs_all_in_core vc.vc
            end
        | RankingFunc(r) -> rhs_all_in_core vc.vc
  in
  let inductive_core_good_enough_for_function function_validity_information = 
    List.for_all important_vc_implies_all_rhs_conjuncts_in_inductive_core function_validity_information.correctness_result.vcs &&
      begin
        match function_validity_information.termination_result with
            None -> true
          | Some(t) ->
              begin
                List.for_all important_vc_implies_all_rhs_conjuncts_in_inductive_core t.decreasing_paths &&                
                  List.for_all important_vc_implies_all_rhs_conjuncts_in_inductive_core t.nonnegative_vcs               
              end
      end
  in
    List.for_all inductive_core_good_enough_for_function function_validity_information_list
      
