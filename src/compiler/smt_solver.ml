open Ast ;;
open Scanner ;;

exception InvalidVC of string ;;
exception InvalidSMTSolver of string ;;
exception InvalidSolverResponse of string;;
exception InvalidSolverCounterexample of string * string ;;

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
    let id = id_of_identifier ident in
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

(* Function to help parse a counterexample from the SMT solver.
   It takes in the function that does the actual work. *)
let parse_counterexample_helper str rev_var_names parser_fn =
  let parts = Str.split (Str.regexp "\n") str in
  let data = parser_fn parts in
  let sort_fn (lhs1, _) (lhs2, _) =
    String.compare (Counterexample.variable_to_string lhs1) (Counterexample.variable_to_string lhs2)
  in
  let sorted_data = List.sort sort_fn data in
  (* print_endline ("Counterexample: " ^ (Counterexample.counterexample_to_string sorted_data)); *)
  sorted_data ;;

(* Convert a VC to the SMT-LIB format (http://combination.cs.uiowa.edu/smtlib/).
   Note that we currently do not support integer division or modulus. *)
let smt_lib_transform_input vc =
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
      | IDiv (loc, t1, t2) -> failwith "I do not know how to express integer division in SMT-LIB" (* TODO: Fix. *)
      | Mod (loc, t1, t2) -> failwith "I do not know how to express modulus in SMT-LIB" (* TODO: Fix. *)
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
    cur_string ^ ":extrafuns ((" ^ name ^ " " ^ smt_lib_string_of_type t ^ "))\n" 
  (* Builds a big string out of all the varDecls in a quantifier we need to define as being specific to that quantifier. *)
  and build_define_string_for_quantifier prev_string cur_decl =
    prev_string ^ "(?" ^ string_of_identifier cur_decl.varName ^ " " ^ smt_lib_string_of_type cur_decl.varType ^ ")"
  in
  let (defines, vc_string, rev_var_names) = transform_input_helper vc smt_lib_string_of_expr build_define_str in
  let smt_lib_string = "(benchmark tmp\n:logic AUFLIA\n" ^ defines ^ ":formula\n" ^ vc_string ^ "\n)\n" in
  (* print_endline smt_lib_string; *)
  (smt_lib_string, rev_var_names) ;;

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
     The first parameter is the function we should use to get a line of output from the SMT solver.
     The second is a helper function to put a newline between two strings. *)
  parse_output : (unit -> string) -> (string -> string -> string) -> string * string option;
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
      let (smt_lib_str, rev_var_names) = smt_lib_transform_input vc in
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
	List.rev_map parse_yices_example parts
      in
      parse_counterexample_helper str rev_var_names parse_whole_counterexample ;;

    let rec parse_output get_line_of_output append =
      try
	(* Gets the counterexample by reading until we hit a newline. *)
	let rec get_counterexample () = match get_line_of_output () with
	  | "" -> ""
	  | x -> append x (get_counterexample ())
	in
	(* Get (response, counterexample opt) from yices. *)
	let recv = get_line_of_output () in
	match recv with
	  | "sat" -> ("sat", Some (get_counterexample ()))
          | "unknown" -> ("unknown", None)
	  | "unsat" -> ("unsat", None)
	  | "Logical context is inconsistent. Use (pop) to restore the previous state." -> assert false
	  | "ok" -> parse_output get_line_of_output append
	  | "" -> ("", None)
	  | _ -> ("", None)
      with
	| End_of_file ->
	    assert false ;;
    
    let get_arguments () =
      [| Filename.basename (Config.get_value "smt_solver_path"); "-e" |] ;; (* -e for evidence (return satisfying model) *)

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
  get_arguments = Yices.get_arguments;
  shutdown_command = Yices.shutdown_command;
} ;;

(* Yices can use SMT-LIB for input too. *)
let yices_solver_using_smt_lib = {
  input_method = Yices.input_method;
  transform_input = Yices.transform_input_smt_lib;
  parse_counterexample = Yices.parse_counterexample;
  parse_output = Yices.parse_output;
  get_arguments = Yices.get_arguments_smt_lib;
  shutdown_command = None;
} ;;

module Z3 =
  struct

    let base_file_name = "z3_input" ;;

    let input_method = File (base_file_name) ;;

    let transform_input vc = smt_lib_transform_input vc ;;
    
    let parse_counterexample str rev_var_names =
      let parse_z3_counterexample parts = 
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
      in
      parse_counterexample_helper str rev_var_names parse_z3_counterexample 

    let parse_output get_line_of_output append =
      let my_get_line_of_output () =
	let line = get_line_of_output () in
	Str.global_replace (Str.regexp "\r") "" line
      in
      try
	(* Gets the counterexample by reading until we hit a newline. *)
	let rec get_counterexample () = match my_get_line_of_output () with
	  | "sat" -> "" (* It prints out sat at the end. *)
	  | "function interpretations:" ->
	      begin
		try
		  while true do
		    print_endline (get_line_of_output ())
		  done;
		  assert false
		with
		  | _ ->
		      assert false (* We don't send free functions to the SMT-solver. *)
	      end
	  | x -> append x (get_counterexample ())
	in
	(* Get (response, counterexample opt) from Z3.. *)
	let recv = my_get_line_of_output () in
	match recv with
	  | "sat" -> ("sat", None)
	  | "partitions:" -> ("sat", Some (get_counterexample ())) (* If there is a counterexample, it comes before the sat. *)
	  | "unsat" -> ("unsat", None)
          | "unknown" -> ("unknown", None)
	  | _ -> raise (InvalidSolverResponse recv)
      with
	| End_of_file ->
	    assert false ;;

    (* Note that you have to append the fifo name to this first. *)
    let get_arguments () =
      [| Filename.basename (Config.get_value "smt_solver_path"); "/m"; "/smt" |] ;; (* /m for model (return satisfying model) *)
    
    let shutdown_command = None ;;

  end ;;

let z3_solver = {
  input_method = Z3.input_method;
  transform_input = Z3.transform_input;
  parse_counterexample = Z3.parse_counterexample;
  parse_output = Z3.parse_output;
  get_arguments = Z3.get_arguments;
  shutdown_command = Z3.shutdown_command;
} ;;

(* Functions that call the solvers *)

let get_smt_solver () =
  let smt_solver_name = Config.get_value "smt_solver_name" in
  match smt_solver_name with
    | "yices" -> yices_solver
    | "z3" -> z3_solver
    | _ -> raise (InvalidSMTSolver smt_solver_name) ;;

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

let get_arguments () =
  let solver = get_smt_solver() in
  solver.get_arguments () ;;

let get_shutdown_command () =
  let solver = get_smt_solver() in
  solver.shutdown_command ;;
