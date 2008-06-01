open Ast ;;

exception InvalidVC of string ;;

let yices_name id name = "__" ^ (string_of_int id) ^ "_" ^ name;;

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
    {name = new_name; location_id = ident.location_id; decl = ident.decl; is_length = ident.is_length} ;;

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
    | Iff (loc, t1, t2) -> Iff (loc, pe t1, pe t2)
    | Implies (loc, t1, t2) -> Implies (loc, pe t1, pe t2)
    | EmptyExpr -> EmptyExpr
  and pl l = parse_lval l var_names rev_var_names
  in
    pe e

(* Transforms an lval by renaming any identifiers in it. *)
and parse_lval lval var_names rev_var_names = match lval with
  | NormLval (loc, id) -> NormLval (loc, rename_and_replace_id id var_names rev_var_names)
  | ArrayLval (loc, arr, e) -> ArrayLval (loc, parse_expr arr var_names rev_var_names, parse_expr e var_names rev_var_names) ;;

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
and build_define_string id (name, t) cur_string =
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
    String.sub str_with_space_at_end 0 ((String.length str_with_space_at_end)-1);;

(* Turns the negation of this VC into a yices-readable string. *)
let transform_for_yices vc =
  
  (* First, find all vars and rename them. *)
  let var_names = Hashtbl.create 10 in
  let rev_var_names = Hashtbl.create 10 in
  (* We negate it since F is valid iff not F is unsat. *)
  let new_vc = parse_expr vc var_names rev_var_names in
  let defines = Hashtbl.fold build_define_string var_names "" in
  let vc_string = yices_string_of_expr new_vc in
  let yices_string = "(set-evidence! true)\n" ^ defines ^ "(assert " ^ vc_string ^ ")\n(check)\n" in
  (yices_string, rev_var_names)
