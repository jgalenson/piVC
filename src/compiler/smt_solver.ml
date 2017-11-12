open Ast ;;
open Scanner ;;

exception InvalidVC of string ;;
exception UnsupportedSMTSolver of string ;;
exception InvalidSolverResponse of string;;
exception InvalidSolverCounterexample of string * string ;;

exception SolverError of string
exception NonLinearProblem
exception SolverTimeout

(* Counterexample stuff *)

module Counterexample =
  struct

    (* A single variable in the counterexample. *)
    type variable =
      | Var of string * identifier
      | ArrayVar of variable * string
      | Div of string * string
      | Mod of string * string ;;

    (* A single part of the counterexample of the form variable=string. *)
    type example = variable * string ;;
    
    let rec variable_to_string v =
      match v with
	| Var (s,_) -> s
	| ArrayVar (v, s) -> (variable_to_string v) ^ "[" ^ s  ^ "]"
	| Div (num, denom) -> num ^ " div " ^ denom
	| Mod (x, y) -> x ^ " mod " ^ y ;;
    
    let example_to_string (lhs, rhs) =
      (variable_to_string lhs) ^ " = " ^ rhs ;;
    
    let counterexample_to_string cx =
      let example_to_string prev ex =
	let new_part = example_to_string ex in
	if prev = "" then new_part else prev ^ "\n" ^ new_part
      in
      List.fold_left example_to_string "" cx ;;
    
    let location_of_example (var,_) =
      let rec location_of_variable v = 
	match v with
	  | Var (_, id) -> Some (Utils.elem_from_opt !(id.decl)).location_vd
	  | ArrayVar (var, _) -> location_of_variable var
	  | _ -> None
      in
      location_of_variable var

  end ;;

(* Generic utilities *)

(* let yices_name id name = "__" ^ (string_of_int id) ^ "_" ^ name;; *)

(* Transforms an expr by renaming any identifiers in it. *)
let rec parse_expr e var_names rev_var_names =
  let rec pe = function
    | Assign (loc, lval, e) -> Assign (loc, pl lval, pe e)
    | Constant (loc, c) -> Constant (loc, c)
    | LValue (loc, lval) -> LValue (loc, pl lval)
    | Call (loc, s, el) -> Call (loc, s, List.map pe el)
    | Plus (loc, t1, t2) -> Plus (loc, pe t1, pe t2)
    | Minus (loc, t1, t2) -> Minus (loc, pe t1, pe t2)
    | Times (loc ,t1, t2) -> Times (loc, pe t1, pe t2)
    | Div (loc, t1, t2) -> Div (loc, pe t1, pe t2)
    | IDiv (loc, t1, t2) -> IDiv (loc, pe t1, pe t2)
    | Mod (loc, t1, t2) -> Mod (loc, pe t1, pe t2)
    | UMinus (loc, t) -> UMinus (loc, pe t)
    | ForAll (loc,decls,e) -> ForAll(loc,decls,pe e)
    | Exists (loc,decls,e) -> Exists(loc,decls,pe e)
    | ArrayUpdate (loc, exp, assign_to, assign_val) -> ArrayUpdate(loc, pe exp, pe assign_to, pe assign_val)
    | LT (loc, t1, t2) -> LT (loc, pe t1, pe t2)
    | LE (loc, t1, t2) -> LE (loc, pe t1, pe t2)
    | GT (loc, t1, t2) -> GT (loc, pe t1, pe t2)
    | GE (loc, t1, t2) -> GE (loc, pe t1, pe t2)
    | EQ (loc, t1, t2) -> EQ (loc, pe t1, pe t2)
    | NE (loc, t1, t2) -> NE (loc, pe t1, pe t2)
    | And (loc, t1, t2) -> And (loc, pe t1, pe t2)
    | Or (loc, t1, t2) -> Or (loc, pe t1, pe t2)
    | Not (loc, t) -> Not (loc, pe t)
    | Length (loc, t) -> Length (loc, pe t)
    | NewArray (loc, t, e, n) -> NewArray (loc, t, pe e, n)
    | Iff (loc, t1, t2) -> Iff (loc, pe t1, pe t2)
    | Implies (loc, t1, t2) -> Implies (loc, pe t1, pe t2)
    | EmptyExpr -> EmptyExpr
  and pl l = parse_lval l var_names rev_var_names
  in
    pe e
      
(* Transforms an lval by renaming any identifiers in it. *)
and parse_lval lval var_names rev_var_names =
  (* Returns a new identifier with a its unique name.
     We make the name based on the id of the identifier.
     We use the var_names hash table to cache these names
     and their associated types. *)
  let rename_and_replace_id ident var_names rev_var_names =
    (* TODO: Hack: yices2 does not accept names beginning with an underscore. *)
    let id =
      let orig_id = id_of_identifier ident in
      if String.length orig_id > 0 && orig_id.[0] = '_' then
	"temp" ^ orig_id
      else
	orig_id
    in
    let new_name = 
      if (Hashtbl.mem var_names id) then
	fst (Hashtbl.find var_names id)
      else
	let new_var_name = id in (*yices_name id ident.name in* TODO-A: delete this commented out line after checking it works*)
	Hashtbl.add var_names id (new_var_name, type_of_identifier ident);
	Hashtbl.add rev_var_names new_var_name ident;
	new_var_name
    in
    {name = new_name; location_id = ident.location_id; decl = ident.decl; is_length = ident.is_length}
  in
  match lval with
    | NormLval (loc, id) -> NormLval (loc, rename_and_replace_id id var_names rev_var_names)
    | ArrayLval (loc, arr, e) -> ArrayLval (loc, parse_expr arr var_names rev_var_names, parse_expr e var_names rev_var_names)
    | _ -> assert(false) ;;

(* Function to help transform input to the SMT solver.
   It takes in the functions that do the actual work. *)
let transform_input_helper vc vc_transform_fn build_define_str_fn =
  (* First, find all vars and rename them. *)
  let var_names = Hashtbl.create 10 in
  let rev_var_names = Hashtbl.create 10 in
  let new_vc = parse_expr vc var_names rev_var_names in
  (* Now we transform the string according to the SMT solver's input interface. *)
  let defines = Hashtbl.fold build_define_str_fn var_names "" in
  let vc_string = vc_transform_fn new_vc in
  (defines, vc_string, rev_var_names) ;;

let sort_counterexamples data = 
  let sort_fn (lhs1, _) (lhs2, _) =
    String.compare (Counterexample.variable_to_string lhs1) (Counterexample.variable_to_string lhs2)
  in
  let sorted_data = List.sort sort_fn data in
  (* print_endline ("Counterexample: " ^ (Counterexample.counterexample_to_string sorted_data)); *)
  sorted_data ;;

(* Function to help parse a counterexample from the SMT solver.
   It takes in the function that does the actual work. *)
let parse_counterexample_helper str rev_var_names parser_fn =
  let parts = Str.split (Str.regexp "\n") str in
  let data = parser_fn parts in
  sort_counterexamples data ;;

(* Convert a VC to the SMT-LIB format (http://combination.cs.uiowa.edu/smtlib/).
   Note that we currently do not support integer division or modulus. *)
let smt_lib_transform_input_helper vc =
  (* Convert our AST to the smt-lib format. *)
  let rec smt_lib_string_of_expr e =
    let rec soe = function
      | Constant (loc, c) ->
	  begin
	    match c with
	      | ConstInt (loc, i) -> string_of_int i
	      | ConstFloat (loc, f) -> string_of_float f
	      | ConstBool (loc, b) -> string_of_bool b
	  end
      | LValue (loc, lval) ->
	  begin
	    match lval with
	      | NormLval (loc, ident) -> ident.name
	      | ArrayLval (loc, arr, e) -> "(select " ^ (soe arr) ^ " " ^ (soe e) ^ ")"
	      | _ -> assert(false)
	  end
      | Plus (loc, t1, t2) -> "(+ " ^ (soe t1) ^ " " ^ (soe t2) ^ ")"
      | Minus (loc, t1, t2) -> "(- " ^ (soe t1) ^ " " ^ (soe t2) ^ ")"
      | Times (loc, t1, t2) -> "(* " ^ (soe t1) ^ " " ^ (soe t2) ^ ")"
      | Div (loc, t1, t2) -> "(/ " ^ (soe t1) ^ " " ^ (soe t2) ^ ")"
      | IDiv (loc, t1, t2) -> failwith "I do not know how to express integer division in SMT-LIB 1" (* TODO: Fix. *)
      | Mod (loc, t1, t2) -> failwith "I do not know how to express modulus in SMT-LIB 1" (* TODO: Fix. *)
      | UMinus (loc, t) -> "(~ " ^ (soe t) ^ ")"
      | ForAll (loc, decls, e) -> "(forall " ^ List.fold_left build_define_string_for_quantifier "" decls ^ " " ^ soe e ^ ")"
      | Exists (loc, decls, e) -> "(exists " ^ List.fold_left build_define_string_for_quantifier "" decls ^ " " ^ soe e ^ ")"
      | ArrayUpdate (loc, exp, assign_to, assign_val) -> "(store " ^ soe exp ^ " " ^ soe assign_to ^ " " ^ soe assign_val ^ ")"
      | LT (loc, t1, t2) -> "(< " ^ (soe t1) ^ " " ^ (soe t2) ^ ")"
      | LE (loc, t1, t2) -> "(<= " ^ (soe t1) ^ " " ^ (soe t2) ^ ")"
      | GT (loc, t1, t2) -> "(> " ^ (soe t1) ^ " " ^ (soe t2) ^ ")"
      | GE (loc, t1, t2) -> "(>= " ^ (soe t1) ^ " " ^ (soe t2) ^ ")"
      | EQ (loc, t1, t2) -> "(= " ^ (soe t1) ^ " " ^ (soe t2) ^ ")"
      | NE (loc, t1, t2) -> "(not (= " ^ (soe t1) ^ " " ^ (soe t2) ^ "))"
      | And (loc, t1, t2) -> "(and " ^ (soe t1) ^ " " ^ (soe t2) ^ ")"
      | Or (loc, t1, t2) -> "(or " ^ (soe t1) ^ " " ^ (soe t2) ^ ")"
      | Not (loc, t) -> "(not " ^ (soe t) ^ ")"
      | Length (loc, t) -> raise (InvalidVC ("Length not yet implemented."))
	  (* TODO: Implement.  Make a variable for each array to be its size?
	     But what if this is the length of say a function that returns an arr or 2d_arr[i]? *)
      | Iff (loc, t1, t2) -> "(iff " ^ (soe t1) ^ " " ^ (soe t2) ^ ")"
      | Implies (loc, t1, t2) -> "(implies " ^ (soe t1) ^ " " ^ (soe t2) ^ ")"
      | EmptyExpr -> ""
      | _ -> raise (InvalidVC ("Unexpected expr type in VC: " ^ (string_of_expr e)))
    in
    soe e 
  and smt_lib_string_of_type t = match t with
    | Bool (loc) -> "Bool"
    | Int (loc) -> "Int"
    | Float (loc) -> "Real"
    | Array (typ, l) -> "Array"
    | _ -> raise (InvalidVC ("Unimplemented type: " ^ string_of_type t)) (* TODO: Finish *)
  (* Builds a big string out of all the variables we need to define. *)
  and build_define_str id (name, t) cur_string =
    let sep = if cur_string = "" then "" else " " in
    cur_string ^ sep ^ "(" ^ name ^ " " ^ smt_lib_string_of_type t ^ ")" 
  (* Builds a big string out of all the varDecls in a quantifier we need to define as being specific to that quantifier. *)
  and build_define_string_for_quantifier prev_string cur_decl =
    prev_string ^ "(?" ^ string_of_identifier cur_decl.varName ^ " " ^ smt_lib_string_of_type cur_decl.varType ^ ")"
  in
  transform_input_helper vc smt_lib_string_of_expr build_define_str ;;

let smt_lib_transform_input_normal vc =
  let (defines, vc_string, rev_var_names) = smt_lib_transform_input_helper vc in
  let defines_str =
    if defines = "" then
      ""
    else
      ":extrafuns (" ^ defines ^ ")\n"
  in
  let smt_lib_string = "(benchmark tmp\n:logic QF_AUFLIA\n" ^ defines_str ^ ":formula\n" ^ vc_string ^ "\n)\n" in
  (* print_endline smt_lib_string; *)
  (smt_lib_string, rev_var_names) ;;

(* For SMTLIBs commandline input format (currently experimental). *)
let smt_lib_transform_input_commandline vc =
  let (defines, vc_string, rev_var_names) = smt_lib_transform_input_helper vc in
  let smt_lib_string = "(declare-funs (" ^ defines ^ "))\n(assert " ^ vc_string ^ ")\n(check-sat)\n(get-info model)\n" in
  (* print_endline smt_lib_string; *)
  (smt_lib_string, rev_var_names) ;;

(* Convert a VC to the SMT-LIB 2 format (http://combination.cs.uiowa.edu/smtlib/).
   Note that we currently do not support integer division or modulus. *)
let smt_lib2_transform_input_helper vc =
  (* Convert our AST to the smt-lib format. *)
  let rec smt_lib_string_of_expr e =
    let rec soe = function
      | Constant (loc, c) ->
	  begin
	    match c with
	      | ConstInt (loc, i) -> string_of_int i
	      | ConstFloat (loc, f) -> string_of_float f
	      | ConstBool (loc, b) -> string_of_bool b
	  end
      | LValue (loc, lval) ->
	  begin
	    match lval with
	      | NormLval (loc, ident) -> ident.name
	      | ArrayLval (loc, arr, e) -> "(select " ^ (soe arr) ^ " " ^ (soe e) ^ ")"
	      | _ -> assert(false)
	  end
      | Plus (loc, t1, t2) -> "(+ " ^ (soe t1) ^ " " ^ (soe t2) ^ ")"
      | Minus (loc, t1, t2) -> "(- " ^ (soe t1) ^ " " ^ (soe t2) ^ ")"
      | Times (loc, t1, t2) -> "(* " ^ (soe t1) ^ " " ^ (soe t2) ^ ")"
      | Div (loc, t1, t2) -> "(/ " ^ (soe t1) ^ " " ^ (soe t2) ^ ")"
      | IDiv (loc, t1, t2) -> "(div " ^ (soe t1) ^ " " ^ (soe t2) ^ ")"
      | Mod (loc, t1, t2) -> "(mod " ^ (soe t1) ^ " " ^ (soe t2) ^ ")"
      | UMinus (loc, t) -> "(~ " ^ (soe t) ^ ")"
      | ForAll (loc, decls, e) -> "(forall " ^ List.fold_left build_define_string_for_quantifier "" decls ^ " " ^ soe e ^ ")"
      | Exists (loc, decls, e) -> "(exists " ^ List.fold_left build_define_string_for_quantifier "" decls ^ " " ^ soe e ^ ")"
      | ArrayUpdate (loc, exp, assign_to, assign_val) -> "(store " ^ soe exp ^ " " ^ soe assign_to ^ " " ^ soe assign_val ^ ")"
      | LT (loc, t1, t2) -> "(< " ^ (soe t1) ^ " " ^ (soe t2) ^ ")"
      | LE (loc, t1, t2) -> "(<= " ^ (soe t1) ^ " " ^ (soe t2) ^ ")"
      | GT (loc, t1, t2) -> "(> " ^ (soe t1) ^ " " ^ (soe t2) ^ ")"
      | GE (loc, t1, t2) -> "(>= " ^ (soe t1) ^ " " ^ (soe t2) ^ ")"
      | EQ (loc, t1, t2) -> "(= " ^ (soe t1) ^ " " ^ (soe t2) ^ ")"
      | NE (loc, t1, t2) -> "(not (= " ^ (soe t1) ^ " " ^ (soe t2) ^ "))"
      | And (loc, t1, t2) -> "(and " ^ (soe t1) ^ " " ^ (soe t2) ^ ")"
      | Or (loc, t1, t2) -> "(or " ^ (soe t1) ^ " " ^ (soe t2) ^ ")"
      | Not (loc, t) -> "(not " ^ (soe t) ^ ")"
      | Length (loc, t) -> raise (InvalidVC ("Length not yet implemented."))
	  (* TODO: Implement.  Make a variable for each array to be its size?
	     But what if this is the length of say a function that returns an arr or 2d_arr[i]? *)
      | Iff (loc, t1, t2) -> "(iff " ^ (soe t1) ^ " " ^ (soe t2) ^ ")"
      | Implies (loc, t1, t2) -> "(implies " ^ (soe t1) ^ " " ^ (soe t2) ^ ")"
      | EmptyExpr -> ""
      | _ -> raise (InvalidVC ("Unexpected expr type in VC: " ^ (string_of_expr e)))
    in
    soe e 
  and smt_lib_string_of_type t = match t with
    | Bool (loc) -> "Bool"
    | Int (loc) -> "Int"
    | Float (loc) -> "Real"
    | Array (typ, l) -> "(Array Int " ^ smt_lib_string_of_type typ ^ ")"
    | _ -> raise (InvalidVC ("Unimplemented type: " ^ string_of_type t)) (* TODO: Finish *)
  (* Builds a big string out of all the variables we need to define. *)
  and build_define_str id (name, t) cur_string =
    let sep = if cur_string = "" then "" else "\n" in
    cur_string ^ sep ^ "(declare-const " ^ name ^ " " ^ smt_lib_string_of_type t ^ ")" 
  (* Builds a big string out of all the varDecls in a quantifier we need to define as being specific to that quantifier. *)
  and build_define_string_for_quantifier prev_string cur_decl =
    prev_string ^ "(?" ^ string_of_identifier cur_decl.varName ^ " " ^ smt_lib_string_of_type cur_decl.varType ^ ")"
  in
  transform_input_helper vc smt_lib_string_of_expr build_define_str ;;

let smt_lib2_transform_input_normal vc =
  let (defines, vc_string, rev_var_names) = smt_lib2_transform_input_helper vc in
  let smt_lib_string = "(set-option :produce-models true)\n" ^ defines ^ "\n(assert " ^ vc_string ^ ")\n(check-sat)\n(get-model)\n" in
  (* print_endline smt_lib_string; *)
  (smt_lib_string, rev_var_names) ;;


let parse_smt_lib2_counterexample str rev_var_names =
  (* Split the counterexamples by matching parentheses to return a list of each top-level example. *)
  let split_counterexample str =
    let rec get_piece str index count =
      let next_index = Str.search_forward (Str.regexp "[()]") str index in
      match Str.matched_group 0 str with
      | "(" -> get_piece str (next_index + 1) (count + 1)
      | ")" when count > 1 -> get_piece str (next_index + 1) (count - 1)
      | ")" when count == 1 -> next_index
      | x -> assert(false)
    in
    let rec get_pieces str index =
      if index >= ((String.length str) - 1) then
        []
      else
        let next_index = get_piece str index 0 in
        let cur_piece = String.sub str index (next_index - index + 1) in
        (String.trim cur_piece) :: (get_pieces str (next_index + 1))
    in
    get_pieces str 0
  in
  let parse_smt_lib2_counterexample parts =
    let replace_name n =
      if (Hashtbl.mem rev_var_names n) then
	(Hashtbl.find rev_var_names n).name
      else
	n
    in
    let array_names = Hashtbl.create 10 in
    (* Parse one line. *)
    (* TODO: This parsing code is ugly, since I have to special case parentheses due to using regexes. *)
    let parse_smt_lib2_example s =
      let rec parse_array arr_var str =
        let trimmed_str = String.trim str in
        if (String.length trimmed_str < 4) || (String.sub trimmed_str 0 4) <> "(ite" then (* Simple value at the end *)
          [(Counterexample.ArrayVar (arr_var, "*"), Str.global_replace (Str.regexp "[()]") "" trimmed_str)]
        else begin (* Array *)
            let (cur_arr_index, second_half) =
              if (Str.string_match (Str.regexp "(ite (= [^ ]* (\\([^)]*\\))) \\(.*\\))$") trimmed_str 0) then
                (Str.matched_group 1 trimmed_str, Str.matched_group 2 trimmed_str)
              else if (Str.string_match (Str.regexp "(ite (= [^ ]* \\([^)]*\\)) \\(.*\\))$") trimmed_str 0) then
                (Str.matched_group 1 trimmed_str, Str.matched_group 2 trimmed_str)
              else
                assert(false)
            in
            let (cur_arr_rhs, rest) = 
              if (Str.string_match (Str.regexp "(\\([^)]*\\)) *\\(.*\\)") second_half 0) then
                (Str.matched_group 1 second_half, Str.matched_group 2 second_half)
              else if (Str.string_match (Str.regexp "\\([^ ]*\\) \\(.*\\)") second_half 0) then
                (Str.matched_group 1 second_half, Str.matched_group 2 second_half)
              else
                assert(false)
            in
            (Counterexample.ArrayVar (arr_var, cur_arr_index), cur_arr_rhs) :: (parse_array arr_var rest)
          end
      in
      let cleaned_str = Str.global_replace (Str.regexp "\n") "" s in
      let regex = Str.regexp "^ *(define-fun \\([^ ]*\\) (\\((?[^)]*)?\\)) [^ ][^0-9(]*\\(.*\\))$" in
      assert (Str.string_match regex cleaned_str 0);
      let lhs_token = Str.matched_group 1 cleaned_str in
      let array_name = Str.matched_group 2 cleaned_str in
      let rhs = Str.matched_group 3 cleaned_str in
      if (Str.string_match (Str.regexp "(_ as-array \\([^)]*\\)") rhs 0) then begin (* Remember SMTLIB2's internal array names. *)
          Hashtbl.add array_names (Str.matched_group 1 rhs) lhs_token;
          []
        end
      else if array_name <> "" then begin (* Array *)
          let cur_arr_name = Hashtbl.find array_names lhs_token in
	  let inner_var = Counterexample.Var (replace_name cur_arr_name, Hashtbl.find rev_var_names cur_arr_name) in
          parse_array inner_var rhs
        end
      else (* Normal value. *)
	let lhs = Counterexample.Var (replace_name lhs_token, Hashtbl.find rev_var_names lhs_token) in
	[(lhs, Str.global_replace (Str.regexp "[()]") "" rhs)]
    in
    List.flatten (List.map parse_smt_lib2_example parts)
  in
  (*print_endline (Hashtbl.fold (fun k v acc -> acc ^ (if acc = "" then "" else ", ") ^ k ^ " -> " ^ (string_of_identifier v)) rev_var_names "");*)
  let model_start = Str.search_forward (Str.regexp "(model") str 0 in
  let model_end = String.rindex str ')' in
  let short_str = String.trim (String.sub str (model_start + 6) (model_end - (model_start + 6))) in
  let parts = split_counterexample short_str in
  let data = parse_smt_lib2_counterexample parts in
  sort_counterexamples data ;;

(* The SMT solver interface. *)

type inputMethod = Stdin | File of string ;;

type smt_solver = {
  (* Whether the solver accepts input on stdin or through a file. *)
  input_method : inputMethod;
  (* Transform our AST representation into a form readable by the SMT solver. *)
  transform_input : expr -> string * (string, Ast.identifier) Hashtbl.t;
  (* Parse the SMT solver's counterexample output. *)
  parse_counterexample : string -> (string, Ast.identifier) Hashtbl.t -> Counterexample.example list;
  (* Parse through the SMT solver's stdout output to return status * counterexample_string option.
     The first parameter is the function we should use to get a line of output from the SMT solver.  Its parameter is whether or not to wait for input.
     The second is a helper function to put a newline between two strings. *)
  parse_output : (bool -> string) -> (string -> string -> string) -> string * string option;
  (* Parse the SMT solver's error output.  If we know what the error is,
     we can throw an exception; otherwise, we can return the text of the error.*)
  parse_error : (unit -> string) -> (string -> string -> string) -> string;
  (* Get the arguments to pass to the SMT solver when we execute it.
     The first argument should be its name. *)
  get_arguments : unit -> string array;
  (* The command we send to the SMT server to exit it, if any. *)
  shutdown_command : string option;
} ;;

(* Definitions for individual SMT solvers *)

module Yices =
  struct

    let input_method = Stdin ;;
    
    (* Turns this VC into a yices-readable string. *)
    let transform_input vc =
      (* Convert our AST to be yices-readable. *)
      let rec yices_string_of_expr e =
	let rec ysoe = function
	  | Constant (loc, c) ->
	      begin
		match c with
		  | ConstInt (loc, i) -> string_of_int i
		  | ConstFloat (loc, f) ->
		      let rational_str = Utils.rational_string_of_float f in
		      let parts = Str.split (Str.regexp "/") rational_str in
		      "(/ " ^ (List.nth parts 0) ^ " " ^ (List.nth parts 1) ^ ")"
		  | ConstBool (loc, b) -> string_of_bool b
	      end
	  | LValue (loc, lval) ->
	      begin
		match lval with
		  | NormLval (loc, ident) -> ident.name
		  | ArrayLval (loc, arr, e) -> "(" ^ (ysoe arr) ^ " " ^ (ysoe e) ^ ")"
		  | _ -> assert(false)
	      end
	  | Plus (loc, t1, t2) -> "(+ " ^ (ysoe t1) ^ " " ^ (ysoe t2) ^ ")"
	  | Minus (loc, t1, t2) -> "(- " ^ (ysoe t1) ^ " " ^ (ysoe t2) ^ ")"
	  | Times (loc, t1, t2) -> "(* " ^ (ysoe t1) ^ " " ^ (ysoe t2) ^ ")"
	  | Div (loc, t1, t2) -> "(/ " ^ (ysoe t1) ^ " " ^ (ysoe t2) ^ ")"
	  | IDiv (loc, t1, t2) -> "(div " ^ (ysoe t1) ^ " " ^ (ysoe t2) ^ ")"
	  | Mod (loc, t1, t2) -> "(mod " ^ (ysoe t1) ^ " " ^ (ysoe t2) ^ ")"
	  | UMinus (loc, t) -> "(- 0 " ^ (ysoe t) ^ ")"
	  | ForAll (loc, decls, e) -> "(forall (" ^ build_define_string_for_quantifier decls ^ ")" ^ ysoe e ^ ")"
	  | Exists (loc, decls, e) -> "(exists (" ^ build_define_string_for_quantifier decls ^ ")" ^ ysoe e ^ ")"
	  | ArrayUpdate (loc, exp, assign_to, assign_val) -> "(update " ^ ysoe exp ^ " (" ^ ysoe assign_to ^ ") " ^ ysoe assign_val ^ ")"
	  | LT (loc, t1, t2) -> "(< " ^ (ysoe t1) ^ " " ^ (ysoe t2) ^ ")"
	  | LE (loc, t1, t2) -> "(<= " ^ (ysoe t1) ^ " " ^ (ysoe t2) ^ ")"
	  | GT (loc, t1, t2) -> "(> " ^ (ysoe t1) ^ " " ^ (ysoe t2) ^ ")"
	  | GE (loc, t1, t2) -> "(>= " ^ (ysoe t1) ^ " " ^ (ysoe t2) ^ ")"
	  | EQ (loc, t1, t2) -> "(= " ^ (ysoe t1) ^ " " ^ (ysoe t2) ^ ")"
	  | NE (loc, t1, t2) -> "(/= " ^ (ysoe t1) ^ " " ^ (ysoe t2) ^ ")"
	  | And (loc, t1, t2) -> "(and " ^ (ysoe t1) ^ " " ^ (ysoe t2) ^ ")"
	  | Or (loc, t1, t2) -> "(or " ^ (ysoe t1) ^ " " ^ (ysoe t2) ^ ")"
	  | Not (loc, t) -> "(not " ^ (ysoe t) ^ ")"
	  | Length (loc, t) -> raise (InvalidVC ("Length not yet implemented."))
	      (* TODO: Implement.  Make a variable for each array to be its size?
		 But what if this is the length of say a function that returns an arr or 2d_arr[i]? *)
	  | Iff (loc, t1, t2) -> "(= " ^ (ysoe t1) ^ " " ^ (ysoe t2) ^ ")"
	  | Implies (loc, t1, t2) -> "(=> " ^ (ysoe t1) ^ " " ^ (ysoe t2) ^ ")"
	  | EmptyExpr -> ""
	  | _ -> raise (InvalidVC ("Unexpected expr type in VC: " ^ (string_of_expr e)))
	in
	ysoe e 
      and yices_string_of_type t = match t with
	| Bool (loc) -> "bool"
	| Int (loc) -> "int"
	| Float (loc) -> "real"
	| Array (typ, l) -> "(-> int " ^ (yices_string_of_type typ) ^ ")"
	| _ -> raise (InvalidVC ("Unimplemented type: " ^ (string_of_type t))) (* TODO: Finish *)
      (* Builds a big string out of all the variables we need to define. *)
      and build_define_str id (name, t) cur_string =
	cur_string ^ "(define " ^ name ^ "::" ^ (yices_string_of_type t) ^ ")\n" 
      (* Builds a big string out of all the varDecls in a quantifier we need to define as being specific to that quantifier. *)
      and build_define_string_for_quantifier decls = 
	let rec space_after_all_elems decls = 
	  match decls with
	      (*TODO-A: delete this comment once working        decl :: rest -> (yices_name (Ast.var_id_of_varDecl decl) (string_of_identifier decl.varName))  ^ "::" ^ yices_string_of_type decl.varType ^ " " ^ space_after_all_elems rest*)
              decl :: rest -> id_of_varDecl decl ^ "::" ^ yices_string_of_type decl.varType ^ " " ^ space_after_all_elems rest
	    | [] -> ""
	in
	let str_with_space_at_end = space_after_all_elems decls in
	String.sub str_with_space_at_end 0 ((String.length str_with_space_at_end)-1)
      in
      let (defines, vc_string, rev_var_names) = transform_input_helper vc yices_string_of_expr build_define_str in
      let yices_string = "(set-evidence! true)\n" ^ defines ^ "(assert " ^ vc_string ^ ")\n(check)\n" in
      (yices_string, rev_var_names) ;;

    let transform_input_smt_lib vc =
      (* We have to send eof/CTRL-D to yices's stdin to tell it we're finished. *)
      let (smt_lib_str, rev_var_names) = smt_lib_transform_input_normal vc in
      let eof = String.make 1 (Char.chr 4) in
      (smt_lib_str ^ eof, rev_var_names) ;;

    let parse_counterexample str rev_var_names =
      let replace_name n =
	if (Hashtbl.mem rev_var_names n) then
	  (Hashtbl.find rev_var_names n).name
	else
	  n
      in
      let get_replaced_token scan =
	replace_name (Scanner.next_token scan)
      in
      let rec parse_counterexample scan =
	let orig_first_token = Scanner.next_token scan in
	let first_token = replace_name orig_first_token in
	if (first_token <> "(") then begin
	  if first_token = "div" then begin (* yices puts in some stupid (div 1 2) nodes when you do integer division. *)
	    let num = Scanner.next_token scan in
	    let denom = Scanner.next_token scan in
	    ignore (Scanner.next_token scan); (* Closing ")" *)
	    Counterexample.Div (num, denom)
	  end else if first_token = "mod" then begin (* yices puts in some stupid (mod 0 2) nodes when you do modulus. *)
	    let x = Scanner.next_token scan in
	    let y = Scanner.next_token scan in
	    ignore (Scanner.next_token scan); (* Closing ")" *)
	    Counterexample.Mod (x, y)
	  end else
            try
	      Counterexample.Var (first_token, Hashtbl.find rev_var_names orig_first_token)
            with 
		Not_found -> raise (InvalidSolverCounterexample (str, orig_first_token))
	end else
	  begin
	    let var = parse_counterexample scan in
	    match var with
	      | Counterexample.Div (_, _) -> var
	      | Counterexample.Mod (_, _) -> var
	      | _ -> (* An array *)
		  let rhs = get_replaced_token scan in
		  let array_var = Counterexample.ArrayVar (var, rhs) in
		  ignore (Scanner.next_token scan); (* Closing ")" *)
		  array_var
	  end
      in
      let parse_whole_counterexample parts =
	(* Parse one line. *)
	let parse_yices_example s =
	  let scan = Scanner.create s in
	  assert (Scanner.next_token scan = "(");
	  assert (Scanner.next_token scan = "=");
	  let lhs = parse_counterexample scan in
	  let rhs = get_replaced_token scan in
	  (lhs, rhs)
	in
	(* TODO: Handle yices2 counterexample arrays better. *)
	let real_parts =
	  let filter_fn s =
	    let scan = Scanner.create s in
	    let first_token = Scanner.next_token scan in
	    first_token <> "MODEL" && first_token <> "---" && first_token <> "default" && first_token <> "----" && first_token <> ""
	  in
	  List.filter filter_fn parts
	in
	List.rev_map parse_yices_example real_parts
      in
      parse_counterexample_helper str rev_var_names parse_whole_counterexample ;;

    let rec parse_output get_line_of_output append =
      try
	(* Gets the counterexample by reading until we hit a newline. *)
	let rec get_counterexample () = match get_line_of_output false with
	  | "" -> ""
	  | x -> append x (get_counterexample ())
	in
	(* Get (response, counterexample opt) from yices. *)
	let recv = get_line_of_output false in
	match recv with
	  | "sat" -> ("sat", Some (get_counterexample ()))
          | "unknown" -> ("unknown", None)
	  | "unsat" -> ("unsat", None)
	  | "Logical context is inconsistent. Use (pop) to restore the previous state." -> assert false
	  | "ok" -> parse_output get_line_of_output append
	  | "" -> ("", None)
	  | _ -> ("", None)
      with
	| End_of_file -> raise (SolverError "Unable to parse SMT solver output.") ;; (* TODO: A better approach would be to read the whole string and then parse it and give the failing string on errors like this. *)

    let rec parse_output_yices2 get_line_of_output append =
      try
	(* Gets the counterexample by reading until we hit a newline. *)
	let rec get_counterexample () =
	  let line =
	    try
	      Some (get_line_of_output false)
	    with
	      | End_of_file -> None
	  in
	  match line with
	    | Some (x) -> append x (get_counterexample ())
	    | None -> ""
	in
	(* Get (response, counterexample opt) from yices. *)
	let recv = get_line_of_output false in
	match recv with
	  | "sat" -> ("sat", Some (get_counterexample ()))
          | "unknown" -> ("unknown", None)
	  | "unsat" -> ("unsat", None)
	  | "Logical context is inconsistent. Use (pop) to restore the previous state." -> assert false
	  | "ok" -> parse_output get_line_of_output append
	  | "" -> ("", None)
	  | _ -> ("", None)
      with
	| End_of_file -> raise (SolverError "Unable to parse SMT solver output.") ;;

    let rec parse_error get_line_of_error append =
      let str = get_line_of_error () in
      if str = "Error: feature not supported: non linear problem." then
	raise NonLinearProblem
      else if str = "Alarm clock" then
	raise SolverTimeout
      else
	str ;;
    
    let get_arguments () =
      (* Note that handling timeouts here is a bit tricky; yices seems to return unknown when it times out sometimes. *)
      [| Filename.basename (Config.get_value "smt_solver_path"); "-e"(*; "-tm"; Config.get_value "timeout_time"*) |] ;; (* -e for evidence (return satisfying model) *)

    let get_arguments_yices2 () =
      [| Filename.basename (Config.get_value "smt_solver_path"); "-m" |] ;;
    
    let get_arguments_smt_lib () =
      Array.append (get_arguments ()) [| "-smt" |] ;;

    let shutdown_command =
      Some ("(quit)\n") ;;
    
  end ;;

let yices_solver = {
  input_method = Yices.input_method;
  transform_input = Yices.transform_input;
  parse_counterexample = Yices.parse_counterexample;
  parse_output = Yices.parse_output;
  parse_error = Yices.parse_error;
  get_arguments = Yices.get_arguments;
  shutdown_command = Yices.shutdown_command;
} ;;

let yices2_solver = {
  input_method = Yices.input_method;
  transform_input = Yices.transform_input_smt_lib;
  parse_counterexample = Yices.parse_counterexample;
  parse_output = Yices.parse_output_yices2;
  parse_error = Yices.parse_error;
  get_arguments = Yices.get_arguments_yices2;
  shutdown_command = None;
} ;;

(* Yices can use SMT-LIB for input too. *)
let yices_solver_using_smt_lib = {
  input_method = Yices.input_method;
  transform_input = Yices.transform_input_smt_lib;
  parse_counterexample = Yices.parse_counterexample;
  parse_output = Yices.parse_output;
  parse_error = Yices.parse_error;
  get_arguments = Yices.get_arguments_smt_lib;
  shutdown_command = None;
} ;;

module Z3 =
  struct

    let base_file_name = "z3_input" ;;

    let input_method_file = File (base_file_name) ;;
    
    let input_method_stdin = Stdin ;;

    let transform_input_file vc = smt_lib_transform_input_normal vc ;;

    let transform_input_file_smt2 vc = smt_lib2_transform_input_normal vc ;;

    let transform_input_stdin vc = smt_lib_transform_input_commandline vc ;;
    
    let parse_counterexample str rev_var_names =
      (*let parse_old_style_z3_counterexample parts = 
	let num_map = Hashtbl.create 10 in
	(* First, build a mapping of numbers to their lines. *)
	let add_to_map line =
	  let space_index = String.index line ' ' in
	  let number = String.sub line 0 space_index in
	  let after_number = String.sub line (space_index + 1) (String.length line - space_index - 1) in
	  Hashtbl.add num_map number after_number
	in
	List.iter add_to_map parts;
	(* Now recursively find the counterexample. *)
	let example_map = Hashtbl.create 10 in
	let value_map = Hashtbl.create 10 in
	(* Parse this line and return its value (RHS).
	   If it represents one (or more) examples, we add it
	   to the example map.  If it represents a simple value,
	   we add it to the value map. *)
	let rec parse_z3_example number rest =
	  (* Recursively get the value (and evaluate) for this number.
	     Since this is only called to recursively evaluate things when
	     we're on the RHS, this number should have a simple value. *)
	  let get_value_for_number number =
	    let rest = Hashtbl.find num_map number in
	    let value_opt = parse_z3_example number rest in
	    assert (Utils.is_some value_opt);
	    Utils.elem_from_opt value_opt
	  in
	  (* Parse named counterexample lines. *)
	  let parse_example scan =
	    let replace_name n =
	      if (Hashtbl.mem rev_var_names n) then
		(Hashtbl.find rev_var_names n).name
	      else
		n
	    in
	    (* A single number can refer to multiple variables, so we get a list
	       of all the names to which this one refers. *)
	    let rec get_names scan =
	      let tok = Scanner.next_token scan in
	      if tok = "}" then
		[]
	      else
		tok :: get_names scan
	    in
	    let names = get_names scan in
	    assert (Scanner.next_token scan = "->");
	    let tok = Scanner.next_token scan in
	    if tok = "{" then begin (* Array *)
	      let rec parse_array scan =
		let tok = Scanner.next_token scan in
		if String.get tok 0 = '*' then begin (* A single mapping. *)
		  let lhs = tok in
		  assert (Scanner.next_token scan = "->");
		  let rhs = Scanner.next_token scan in
		  assert (Scanner.next_token scan = ";");
		  let lhs_value = get_value_for_number lhs in (* index *)
		  let rhs_value = get_value_for_number rhs in (* value *)
		  let make_example name =
		    let inner_var = Counterexample.Var (replace_name name, Hashtbl.find rev_var_names name) in
		    (Counterexample.ArrayVar (inner_var, lhs_value), rhs_value)
		  in
		  let cur_examples = List.rev_map make_example names in
		  cur_examples @ parse_array scan
		end else if tok = "else" then begin (* anything else / wildcard *)
		  assert (Scanner.next_token scan = "->");
		  let rhs = Scanner.next_token scan in
		  let rhs_value = get_value_for_number rhs in (* value *)
		  assert (Scanner.next_token scan = "}");
		  let make_example name =
		    let inner_var = Counterexample.Var (replace_name name, Hashtbl.find rev_var_names name) in
		    (Counterexample.ArrayVar (inner_var, "*"), rhs_value)
		  in
		  List.rev_map make_example names
		end else
		  assert false
	      in
	      let examples = parse_array scan in
	      Hashtbl.add example_map number examples;
	      None
	    end else begin (* Simple RHS. *)
	      assert (Scanner.next_token scan = ":");
	      (* Make one Counterexample.Var for each name this line represents. *)
	      let make_example name = 
		(Counterexample.Var (replace_name name, Hashtbl.find rev_var_names name), tok)
	      in
	      let examples = List.rev_map make_example names in
	      Hashtbl.add example_map number examples;
	      Hashtbl.add value_map number tok;
	      Some (tok)
	    end
	  in
	  (* First, see if we've already computed this. *)
	  if Hashtbl.mem value_map number then
	    Some (Hashtbl.find value_map number)
	  else begin
	    assert (not (Hashtbl.mem example_map number));  (* Only arrays are in example_map but not value_map, and we should never evaluate them more than once since they should never be in a RHS. *)
	    let scan = Scanner.create rest in
	    let token = Scanner.next_token scan in
	    if token = "{" then (* Named variables. *)
	      parse_example scan
	    else begin (* Simple values. *)
	      assert (token = "->");
	      let value = Scanner.next_token scan in
	      assert (Scanner.next_token scan = ":");
	      Hashtbl.add value_map number value;
	      Some (value)
	    end
	  end
	in
	let iter_fn k v =
	  ignore (parse_z3_example k v)
	in
	Hashtbl.iter iter_fn num_map;
	(* Finally, combine them all to get our result. *)
	let add_examples number examples result_list =
	  examples @ result_list
	in
	Hashtbl.fold add_examples example_map []
      in*)
      let parse_z3_counterexample parts =
	let replace_name n =
	  if (Hashtbl.mem rev_var_names n) then
	    (Hashtbl.find rev_var_names n).name
	  else
	    n
	in
        let array_names = Hashtbl.create 10 in
        let cur_arr_name = ref None in
	(* Parse one line. *)
	let parse_z3_example s =
	  (* TODO: Handle new-style z3 counterexample arrays (they use update notation).  I probably want to use the commented code below instead of the scanner code below that. *)
	  (*let scan = Scanner.create s in
	  let lhs_token = Scanner.next_token scan in
	  print_endline lhs_token;
	  let lhs = Counterexample.Var (replace_name lhs_token, Hashtbl.find rev_var_names lhs_token) in
	  assert (Scanner.next_token scan = "->");
	  let rhs = Scanner.rest_str scan in
	  (lhs, rhs)*)
	  let regex = Str.regexp "^ *(?\\([^)]*\\))? -> (?\\([^)}]*\\))?}?$" in
	  assert (Str.string_match regex s 0);
	  let lhs_token = Str.matched_group 1 s in
	  let rhs = Str.matched_group 2 s in
          (*print_endline ("Line: " ^ s ^ ", lhs: " ^ lhs_token ^ ", rhs: " ^ rhs);*)
          if rhs = "{" then begin (* Begin an array. *)
              cur_arr_name := Some(Hashtbl.find array_names lhs_token);
              None
            end
          else if rhs = "}" then begin (* End an array. *)
              cur_arr_name := None;
              None
            end
          else if (Str.string_match (Str.regexp "_ as-array \\(.*\\)$") rhs 0) then begin (* Remember Z3's internal array names. *)
              Hashtbl.add array_names (Str.matched_group 1 rhs) lhs_token;
              None
            end
          else if (Utils.is_some !cur_arr_name) then begin (* Array index. *)
              let name = Utils.elem_from_opt !cur_arr_name in
	      let inner_var = Counterexample.Var (replace_name name, Hashtbl.find rev_var_names name) in
              let lhs_index = match lhs_token with
                | "else" -> "*"
                | lhs_token -> lhs_token
              in
	      Some (Counterexample.ArrayVar (inner_var, lhs_index), rhs)
            end
          else (* Normal value. *)
	    let lhs = Counterexample.Var (replace_name lhs_token, Hashtbl.find rev_var_names lhs_token) in
	    Some (lhs, rhs)
	in
	(* TODO: Hack to get around Z3's current multi-line output. *)
	let new_parts = List.rev (List.fold_left (fun acc cur -> if (try (Str.search_forward (Str.regexp "->") cur 0) >= 0 with | Not_found -> false) then cur :: acc else ((String.sub (List.hd acc) 0 ((String.length (List.hd acc)) - 1)) ^ cur) :: (List.tl acc)) [] parts) in
	let result_opts = List.rev_map parse_z3_example new_parts in
        List.map (fun opt -> Utils.elem_from_opt opt) (List.filter (fun opt -> Utils.is_some opt) result_opts)
      in
      (*print_endline (Hashtbl.fold (fun k v acc -> acc ^ (if acc = "" then "" else ", ") ^ k ^ " -> " ^ (string_of_identifier v)) rev_var_names "");*)
      (* Needed for commandline format. *)
      (*let regex = Str.regexp "^((\"model\" \"\\(\\(.\\|\n\\)*\\)\"))$" in
      assert (Str.string_match regex str 0);
      let real_str = Str.matched_group 1 str in*)
      parse_counterexample_helper str rev_var_names parse_z3_counterexample

    let rec parse_output get_line_of_output append =
      let my_get_line_of_output should_wait =
	let line = get_line_of_output should_wait in
	Str.global_replace (Str.regexp "\r") "" line
      in
      (* Get the counterexample by reading until we hit the end of input. *)
      let rec get_counterexample should_wait =
        try
          match my_get_line_of_output should_wait with
	    | "" -> ""
	    | x -> append x (get_counterexample true)
        with
          | End_of_file -> ""
	in
	(* Get (response, counterexample opt) from Z3. *)
	let rec get_output () =
          try
	    let recv = my_get_line_of_output false in
	    match recv with
	      | "sat" -> ("sat", Some (get_counterexample true))
	      | "unsat" -> ("unsat", None)
              | "unknown" -> ("unknown", None)
	      | "success" -> get_output ()
	      | _ -> ("", None)
          with
	    | End_of_file -> raise (SolverError "Unable to parse SMT solver output.")
	in
	get_output () ;;

    let rec parse_error get_line_of_error append =
      get_line_of_error () ;;

    (* Note that you have to append the fifo name to this first. *)
    let get_arguments_file () =
      [| Filename.basename (Config.get_value "smt_solver_path"); "-smt" |] ;;
    
    let get_arguments_file_smt2 () =
      [| Filename.basename (Config.get_value "smt_solver_path"); "-smt2" |] ;;
    
    let get_arguments_stdin () =
      [| Filename.basename (Config.get_value "smt_solver_path"); "-smt"; "-in" |] ;;

    let shutdown_command_file = None ;;
    
    let shutdown_command_stdin = 
      Some ("(exit)\n") ;;

  end ;;

let z3_solver = {
  input_method = Z3.input_method_file;
  transform_input = Z3.transform_input_file;
  parse_counterexample = Z3.parse_counterexample;
  parse_output = Z3.parse_output;
  parse_error = Z3.parse_error;
  get_arguments = Z3.get_arguments_file;
  shutdown_command = Z3.shutdown_command_file;
} ;;

let z3_solver_smtlib2 = {
  input_method = Z3.input_method_file;
  transform_input = Z3.transform_input_file_smt2;
  parse_counterexample = parse_smt_lib2_counterexample;
  parse_output = Z3.parse_output;
  parse_error = Z3.parse_error;
  get_arguments = Z3.get_arguments_file_smt2;
  shutdown_command = Z3.shutdown_command_file;
} ;;

(* Currently experimental. *)
let z3_solver_stdin = {
  input_method = Z3.input_method_stdin;
  transform_input = Z3.transform_input_stdin;
  parse_counterexample = Z3.parse_counterexample;
  parse_output = Z3.parse_output;
  parse_error = Z3.parse_error;
  get_arguments = Z3.get_arguments_stdin;
  shutdown_command = Z3.shutdown_command_stdin;
} ;;

(* Functions that call the solvers *)

let get_smt_solver () =
  let smt_solver_name = Config.get_value "smt_solver_name" in
  match smt_solver_name with
    | "yices" -> yices_solver
    | "yices2" -> yices2_solver
    | "yices-smtlib" -> yices_solver_using_smt_lib
    | "z3" -> z3_solver_smtlib2
    | _ -> raise (UnsupportedSMTSolver smt_solver_name) ;;

let get_input_method () =
  let solver = get_smt_solver() in
  solver.input_method ;;

let transform_input vc =
  let solver = get_smt_solver() in
  solver.transform_input vc ;;

let parse_counterexample str rev_var_names =
  let solver = get_smt_solver() in
  solver.parse_counterexample str rev_var_names ;;

let parse_output get_line_of_output =
  let solver = get_smt_solver() in
  (* Check to make sure we don't put \n at end of returned string. *)
  let append a b =
    if b = "" then a else (a ^ "\n" ^ b)
  in
  solver.parse_output get_line_of_output append ;;

let parse_error get_line_of_output =
  let solver = get_smt_solver() in
  (* Check to make sure we don't put \n at end of returned string. *)
  let append a b =
    if b = "" then a else (a ^ "\n" ^ b)
  in
  solver.parse_error get_line_of_output append ;;

let get_arguments () =
  let solver = get_smt_solver() in
  solver.get_arguments () ;;

let get_shutdown_command () =
  let solver = get_smt_solver() in
  solver.shutdown_command ;;
