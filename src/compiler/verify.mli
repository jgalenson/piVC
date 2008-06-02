open Semantic_checking ;;
open Ast ;;

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
and verification_atom = {
  vc : vc_conjunct list list;
  bp : Basic_paths.basic_path option; (*Nonnegative VCs don't have basic paths*)
  valid : validity;
  counter_example : Counterexamples.example list option;
}
and vc_conjunct = {
  exp : expr;
  valid_conjunct : validity option; (*non-rhs conjuncts don't have a validity*)
  counter_example_conjunct : Counterexamples.example list option;
  in_inductive_core : (bool ref) option;
}
and function_validity_information = {
  fn : fnDecl;
  termination_result : termination_result option;
  correctness_result : correctness_result;
  overall_validity : validity;
}
and verification_atom_temp = {
  func_temp: fnDecl;
  vc_temp : vc_conjunct list list;
  bp_temp : Basic_paths.basic_path option;
}

val name_of_verification_atom : verification_atom -> string ;;
    
val get_all_info : program -> Utils.options -> (Ast.fnDecl * (Basic_paths.basic_path * expr) list * (Basic_paths.basic_path * expr) list * expr list) list

val string_of_validity : validity -> string 


val verify_program : (Ast.fnDecl * (Basic_paths.basic_path * expr) list  * (Basic_paths.basic_path * expr) list * expr list) list
                      -> Ast.program -> ((string, ((validity * Counterexamples.example list option) * float)) Hashtbl.t * Mutex.t)                      
                      -> Utils.options
                      -> function_validity_information list

val inductive_core_good_enough : function_validity_information list -> bool

val overall_validity_of_vc_detailed_list : verification_atom list -> validity

val location_of_vc_conjunct_list_list : vc_conjunct list list -> location

type 'a thread_response =
  | Normal of 'a
  | Exceptional of exn ;;

val verify_vc_expr :
  Ast.expr *
  ((string, (validity * Counterexamples.example list option) * float)
     Hashtbl.t * Mutex.t) *
  Ast.program -> (validity * Counterexamples.example list option) thread_response


val verify_vc : verification_atom_temp *
  ((string, (validity * Counterexamples.example list option) * float)
     Hashtbl.t * Mutex.t) *
  Ast.program * verification_mode -> verification_atom_temp thread_response
    

val overall_validity_of_function_validity_information_list : function_validity_information list -> validity ;;
