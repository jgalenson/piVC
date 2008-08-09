open Ast

exception CantReplaceLValueWithExpr
exception NotLValueExpr
exception InvalidFormula
exception OutsideFragment

(* CODE SECTION: SUBSTITUTING VARIABLE NAMES IN EXPRS *)


let rec array_ident_from_lval lval = match lval with
    NormLval(loc,id) -> id
  | ArrayLval(loc,arr,index) -> 
      begin
        let rec array_name_from_expr arr = 
          match arr with
              LValue(loc,lval) -> array_ident_from_lval lval
            | ArrayUpdate(loc,expr,assign_to,assign_val) -> array_name_from_expr expr
            | _ -> assert(false)
        in array_name_from_expr arr
      end

(* The way the user expresses array-writes is different to how the VC expresses array updates.
   As an example, observe that the following two expressions are equivilent:
   1. q[1][2][3]=4;
   2. q := q{1<-q[1]{2<-q[1][2]{3<-4}}};
   This function converts exprs from type 1 into type 2.
*)
let rec array_write_to_array_update lhs rhs =
  match lhs with
      LValue(loc,l) ->
        begin
          match l with
              NormLval(loc,id) -> Assign(get_dummy_location (), l, rhs)
            | ArrayLval(loc,arr,index) ->
                let new_rhs = ArrayUpdate(get_dummy_location (), arr, index, rhs) in
                  array_write_to_array_update arr new_rhs 
        end
    | _ -> assert(false) 

(* Substitutes variable names, but does not substitue function/predicate names.*)
(*let rec sub_idents_in_expr expr ident_subs = 
(*  let rec get_new_ident_subs old_ident_subs decls = 
    match old_ident_subs with
        sub :: subs -> 
          begin
            let sub_matches_decl sub decl = 
              (String.compare (fst sub) decl.varName.name)==0
            in
            match List.filter (sub_matches_decl sub) decls with
                [] -> sub :: get_new_ident_subs subs decls
              | _ -> get_new_ident_subs subs decls
          end
      | _ -> []
  in*)
  let get_match_sub ident ident_subs = 
    let sub_to_return = ref None in
    let replacement_func possibility = match (String.compare (id_of_identifier ident) (id_of_identifier (fst possibility))) with
	0 -> sub_to_return := Some(snd possibility)
      | _ -> ignore ()
    in
      List.iter replacement_func ident_subs;
      !sub_to_return
  in
  let rec sub_idents_in_expr_list expr_list = 
    match expr_list with 
        [] -> []
      | e::l -> sub_idents_in_expr e ident_subs :: sub_idents_in_expr_list l
  in
  let rec sub expr = 
    match expr with
      | Assign (loc,l, e) ->
          begin
            match l with
                NormLval(loc,id) ->
                  begin
                    let match_sub = get_match_sub id ident_subs in
                      begin
                        match match_sub with
                            Some(replacement) ->
                              begin
                                match replacement with
                                    LValue(loc,l) -> Assign(loc,l,sub e)
                                  | _ -> raise NotLValueExpr
                              end
                          | None -> Assign(loc,l,sub e)
                      end
                  end
              | ArrayLval(loc,arr,index) -> Assign(loc,ArrayLval(loc,sub arr, sub index),sub e)
          end
      | Constant (loc,c) -> expr
      | LValue (loc,l) -> 
          begin
            match l with
                NormLval(loc,id) ->
                  begin
                    let match_sub = get_match_sub id ident_subs in
                      match match_sub with
                          Some(sub) -> sub
                        | None -> expr
                  end
              | ArrayLval(loc,arr,index) -> LValue(loc,ArrayLval(loc,sub arr,sub index))
          end
      | Call (loc,s, el) -> Call(loc, s, sub_idents_in_expr_list el)
      | Plus (loc,t1, t2) -> Plus(loc, sub t1, sub t2)
      | Minus (loc,t1, t2) -> Minus(loc, sub t1, sub t2)
      | Times (loc,t1, t2) -> Times(loc, sub t1, sub t2)
      | Div (loc,t1, t2) -> Div(loc, sub t1, sub t2)
      | IDiv (loc,t1, t2) -> IDiv(loc, sub t1, sub t2)
      | Mod (loc,t1, t2) -> Mod(loc, sub t1, sub t2) 
      | UMinus (loc,t) -> UMinus(loc, sub t)
      | ForAll (loc,decls,e) -> ForAll(loc,decls,sub_idents_in_expr e ident_subs)
      | Exists (loc,decls,e) -> Exists(loc,decls,sub_idents_in_expr e ident_subs)
      | ArrayUpdate (loc,expr,assign_to,assign_val) -> ArrayUpdate(loc, sub expr, sub assign_to, sub assign_val)
      | LT (loc,t1, t2) -> LT(loc, sub t1, sub t2)
      | LE (loc,t1, t2) -> LE(loc, sub t1, sub t2)
      | GT (loc,t1, t2) -> GT(loc, sub t1, sub t2)
      | GE (loc,t1, t2) -> GE(loc, sub t1, sub t2)
      | EQ (loc,t1, t2) -> EQ(loc, sub t1, sub t2)
      | NE (loc,t1, t2) -> NE(loc, sub t1, sub t2)
      | And (loc,t1, t2) -> And(loc, sub t1, sub t2)
      | Or (loc,t1, t2) -> Or(loc, sub t1, sub t2)
      | Not (loc,t) -> Not(loc, sub t)
      | Iff (loc,t1, t2) -> Iff(loc, sub t1, sub t2)
      | Implies (loc,t1, t2) -> Implies(loc, sub t1, sub t2)
      | Length (loc, t) -> Length(loc, sub t)
      | EmptyExpr  -> expr
  in 
    sub expr ;;
*)

let sub_idents expr fn =
  let lval_of_expr e = match e with
    | LValue (loc, l) -> l
    | _ -> assert false
  in
  let rec sub expr = match expr with
    | Assign (loc,l, e) -> Assign (loc, lval_of_expr (sub_lval l), sub e)
    | Constant (loc,c) -> expr
    | LValue (loc,l) -> sub_lval l
    | Call (loc,s, el) -> Call(loc, s, List.map sub el)
    | Plus (loc,t1, t2) -> Plus(loc, sub t1, sub t2)
    | Minus (loc,t1, t2) -> Minus(loc, sub t1, sub t2)
    | Times (loc,t1, t2) -> Times(loc, sub t1, sub t2)
    | Div (loc,t1, t2) -> Div(loc, sub t1, sub t2)
    | IDiv (loc,t1, t2) -> IDiv(loc, sub t1, sub t2)
    | Mod (loc,t1, t2) -> Mod(loc, sub t1, sub t2) 
    | UMinus (loc,t) -> UMinus(loc, sub t)
    | ForAll (loc,decls,e) -> ForAll(loc, decls, sub e)
    | Exists (loc,decls,e) -> Exists(loc, decls, sub e)
    | ArrayUpdate (loc,expr,assign_to,assign_val) -> ArrayUpdate(loc, sub expr, sub assign_to, sub assign_val)
    | LT (loc,t1, t2) -> LT(loc, sub t1, sub t2)
    | LE (loc,t1, t2) -> LE(loc, sub t1, sub t2)
    | GT (loc,t1, t2) -> GT(loc, sub t1, sub t2)
    | GE (loc,t1, t2) -> GE(loc, sub t1, sub t2)
    | EQ (loc,t1, t2) -> EQ(loc, sub t1, sub t2)
    | NE (loc,t1, t2) -> NE(loc, sub t1, sub t2)
    | And (loc,t1, t2) -> And(loc, sub t1, sub t2)
    | Or (loc,t1, t2) -> Or(loc, sub t1, sub t2)
    | Not (loc,t) -> Not(loc, sub t)
    | Iff (loc,t1, t2) -> Iff(loc, sub t1, sub t2)
    | Implies (loc,t1, t2) -> Implies(loc, sub t1, sub t2)
    | Length (loc, t) -> Length(loc, sub t)
    | NewArray(loc,t,e) -> NewArray(loc,t,sub e)
    | EmptyExpr  -> expr
  and sub_lval lval = match lval with
    | NormLval (loc, id) ->
	begin
	  let result = fn id in
	    match result with
	      | Some (e) -> e
	      | None -> LValue (loc, NormLval (loc, id))
	end
    | ArrayLval (loc, arr, index) -> LValue (loc, ArrayLval (loc, sub arr, sub index))
  in
    sub expr ;;

let rec sub_idents_in_expr_while_preserving_original_location expr ident_subs =
  let get_new_ident ident =
    try
      let is_same (old, replace) =
	((id_of_identifier ident) = (id_of_identifier old)) && (ident.name = old.name)
      in
      Some (replace_loc_of_expr (snd (List.find is_same ident_subs)) ident.location_id)
        (*we want to keep the old location, so we call replace_loc_of_expr*)
    with
      Not_found -> None
  in
  sub_idents expr get_new_ident ;;

(* returns a list of the identifier names of the function's arguments *)
let get_idents_of_formals func = 
  let rec build_ident_list remaining = 
    match remaining with
        [] -> []
      | e :: l -> e.varName :: build_ident_list l
  in 
    build_ident_list func.formals


(*Changes the quantification e.g. from a forall to an exists. Useful for getting rid of negations etc.*)
let change_quantifier old_decls old_expr new_quant =
  (*We also need to change the decls, because the quantification is a property of the decls.*)
  let old_decl_to_new_decl decl = 
    {varType=decl.varType; varName=decl.varName; location_vd=decl.location_vd; var_id = decl.var_id; quant = new_quant; is_param = decl.is_param;}
  in
  let new_decls = List.map old_decl_to_new_decl old_decls in
  let get_replacement_pair old_decl new_decl = 
    (old_decl.varName,LValue(gdl(),NormLval(gdl(),{name = new_decl.varName.name; location_id = gdl(); decl = ref (Some(new_decl)); is_length = false;})))
  in
  let replacement_pairs = List.map2 get_replacement_pair old_decls new_decls in
  let new_expr = sub_idents_in_expr_while_preserving_original_location old_expr replacement_pairs in
    (new_decls,new_expr)
    
      
let rec cnf expr = 
  nnf_to_cnf (nnf expr)
    
and nnf_to_cnf expr =
  match expr with
    | And(loc,t1,t2) ->
        begin
          let (t1,t2) = (nnf_to_cnf t1, nnf_to_cnf t2) in
            match t1 with
              | Or(loc,u1,u2) -> Or(gdl(),And(gdl(),u1,t2),And(gdl(),u2,t2))
              | _ -> 
                  begin
                    match t2 with
                      | Or(loc,u1,u2) -> Or(gdl(),And(gdl(),t1,u1),And(gdl(),t1,u2))
                      | _ -> expr
                  end
        end
    | Or(loc,t1,t2) -> Or(gdl(),nnf_to_cnf t1, nnf_to_cnf t2)
    | ForAll (loc,decls,e) -> ForAll(loc,decls,nnf_to_cnf e)
    | Exists (loc,decls,e) -> Exists(loc,decls,nnf_to_cnf e)
    | _ -> expr
        
and nnf expr = 
  match expr with
    | Assign (loc,l, e) -> assert(false)
    | Constant (loc,c) -> expr
    | LValue (loc,l) -> expr
    | Call (loc,s, el) -> assert(false)
    | Plus (loc,t1, t2) -> Plus(loc, nnf t1, nnf t2)
    | Minus (loc,t1, t2) -> Minus(loc, nnf t1, nnf t2)
    | Times (loc,t1, t2) -> Times(loc, nnf t1, nnf t2)
    | Div (loc,t1, t2) -> Div(loc, nnf t1, nnf t2)
    | IDiv (loc,t1, t2) -> IDiv(loc, nnf t1, nnf t2)
    | Mod (loc,t1, t2) -> Mod(loc, nnf t1, nnf t2)
    | UMinus (loc,t) -> UMinus(loc, nnf t)
    | ForAll (loc,decls,e) -> ForAll(loc,decls,e)
    | Exists (loc,decls,e) -> Exists(loc,decls,e)
    | ArrayUpdate (loc,expr,assign_to,assign_val) -> ArrayUpdate(loc, nnf expr, nnf assign_to, nnf assign_val)
    | LT (loc,t1, t2) -> LT(loc, nnf t1, nnf t2)
    | LE (loc,t1, t2) -> LE(loc, nnf t1, nnf t2)
    | GT (loc,t1, t2) -> GT(loc, nnf t1, nnf t2)
    | GE (loc,t1, t2) -> GE(loc, nnf t1, nnf t2)
    | EQ (loc,t1, t2) -> EQ(loc, nnf t1, nnf t2)
    | NE (loc,t1, t2) -> NE(loc, nnf t1, nnf t2)
    | And (loc,t1, t2) -> And(loc, nnf t1, nnf t2)
    | Or (loc,t1, t2) -> Or(loc, nnf t1, nnf t2)
    | Not (loc,t) ->
        begin
          let nnf_t = nnf t in
            match nnf_t with
                Not(loc2,t2) -> nnf t2
              | Constant(loc2, con) ->
                  begin
                    match con with
                        ConstBool(loc,b) -> Constant(loc2, (ConstBool(loc2,not b)))
                      | _ -> raise InvalidFormula
                  end
              | ForAll(loc2,decls,e) ->
                  begin
                    let (new_decls,new_expr) = change_quantifier decls e Existential in
                      Exists(loc2,new_decls,nnf (Not(loc2,nnf new_expr)))
                  end
              | Exists(loc2,decls,e) ->
                  begin
                    let (new_decls,new_expr) = change_quantifier decls e Universal in
                      ForAll(loc2,new_decls,nnf (Not(loc2,nnf new_expr)))
                  end
              | And(loc2,t1,t2) -> Or(loc,nnf (Not(loc, nnf t1)), nnf (Not(loc,nnf t2)))
              | Or(loc2,t1,t2) -> And(loc,nnf (Not(loc, nnf t1)), nnf (Not(loc,nnf t2)))
              | _ -> Not(loc, nnf_t)
        end
    | Implies (loc,t1, t2) -> Or(loc,nnf (Not(loc, nnf t1)), nnf t2)
    | Iff (loc,t1, t2) ->
        begin
          let nnf_t1 = nnf t1 in
          let nnf_t2 = nnf t2 in
            And(loc,nnf (Implies(loc,nnf_t1, nnf_t2)),nnf (Implies(loc,nnf_t2, nnf_t1)))
        end
    | Length (loc, t) -> Length(loc, nnf t)
    | NewArray (loc, t,e) -> assert(false)
    | EmptyExpr  -> expr




let string_of_expr_list exprs delimiter = 
  let rec delimiter_after_all_elems exprs = 
    match exprs with
        expr :: rest -> string_of_expr expr ^ delimiter ^ delimiter_after_all_elems rest
      | [] -> ""
  in
  let str_with_delimiter_at_end = delimiter_after_all_elems exprs in
    String.sub str_with_delimiter_at_end 0 ((String.length str_with_delimiter_at_end)-(String.length delimiter))

(*The strings (guaranteed_unique_string_of_expr a) and (guaranteed_unique_string_of_expr b) will
  be equal iff exprs a and b are equal.
  This differs from the standard string_of_expr, which will return identical strings
  for overloaded variables.
*)
let guaranteed_unique_string_of_expr e =
  let rec unique_string_of_lval lval = match lval with
    | NormLval (loc, s) -> id_of_identifier s
    | ArrayLval (loc, t1, t2) -> (soe t1) ^ "[" ^ (soe t2) ^ "]"
  and unique_representation_of_decl_names decls = 
    let rec construct_str decls = 
      match decls with
          decl :: rest -> (id_of_varDecl decl) ^ "," ^ construct_str rest
        | [] -> ""
    in construct_str decls
  and unique_string_of_type t = 
    match t with
        Identifier(id,loc) -> id_of_identifier id 
      | _ -> string_of_type t
  and soe = function
    | Assign (loc,l, e) -> (unique_string_of_lval l) ^ " := " ^ (soe e)
    | Constant (loc,c) -> (string_of_constant c)
    | LValue (loc,l) -> (unique_string_of_lval l)
    | Call (loc,s, el) -> string_of_identifier s ^ "(" ^ (String.concat ", " (List.map soe el)) ^ ")"
    | Plus (loc,t1, t2) -> (soe t1) ^ " + " ^ (soe t2)
    | Minus (loc,t1, t2) -> (soe t1) ^ " - " ^ (soe t2)
    | Times (loc,t1, t2) -> (soe t1) ^ " * " ^ (soe t2)
    | Div (loc,t1, t2) -> (soe t1) ^ " / " ^ (soe t2)
    | IDiv (loc,t1, t2) -> (soe t1) ^ " div " ^ (soe t2)					       
    | Mod (loc,t1, t2) -> (soe t1) ^ " % " ^ (soe t2)
    | UMinus (loc,t) -> "-" ^ (soe t)
    | ForAll (loc,decls,e) -> "forall " ^ unique_representation_of_decl_names decls ^ ".(" ^ soe e ^ ")"
    | Exists (loc,decls,e) -> "exists " ^ unique_representation_of_decl_names decls ^ ".(" ^ soe e ^ ")"
    | ArrayUpdate (loc,exp,assign_to,assign_val) -> soe exp ^ "{" ^ soe assign_to ^ " <- " ^ soe assign_val ^ "}"
    | LT (loc,t1, t2) -> (soe t1) ^ " < " ^ (soe t2)
    | LE (loc,t1, t2) -> (soe t1) ^ " <= " ^ (soe t2)
    | GT (loc,t1, t2) -> (soe t1) ^ " > " ^ (soe t2)
    | GE (loc,t1, t2) -> (soe t1) ^ " >= " ^ (soe t2)
    | EQ (loc,t1, t2) -> (soe t1) ^ " = " ^ (soe t2)
    | NE (loc,t1, t2) -> (soe t1) ^ " != " ^ (soe t2)
    | And (loc,t1, t2) -> "(" ^ (soe t1) ^ ") && (" ^ (soe t2) ^ ")"
    | Or (loc,t1, t2) -> "(" ^ (soe t1) ^ ") || (" ^ (soe t2) ^ ")"
    | Not (loc,t) -> "!(" ^ (soe t) ^ ")"
    | Iff (loc,t1, t2) -> "(" ^ (soe t1) ^ ") <-> (" ^ (soe t2) ^ ")"
    | Implies (loc,t1, t2) -> "(" ^ (soe t1) ^ ") -> (" ^ (soe t2) ^ ")"
    | Length (loc, t) -> "|" ^ (soe t) ^ "|"
    | NewArray(loc,t,e) -> "new " ^ unique_string_of_type t ^ "[" ^ soe e ^ "]"
    | EmptyExpr  -> ""
  in
  soe e


let conjuncts_of_exprs exprs = 
  let rec coe exprs = 
    match List.length exprs with
        0 -> EmptyExpr
      | 1 -> List.hd exprs
      | _ -> And(gdl(),List.hd exprs,coe (List.tl exprs))
  in coe exprs
    

module Expr_set = Set.Make (struct
                              type t = expr
                              let compare exp1 exp2 = String.compare (guaranteed_unique_string_of_expr exp1) (guaranteed_unique_string_of_expr exp2)
                            end)
let remove_duplicates_from_list exprs = 
  let rec build_set so_far remaining_exprs = 
    match remaining_exprs with 
        [] -> so_far
      | exp :: exps -> build_set (Expr_set.add exp so_far) exps
  in 
  let s = build_set Expr_set.empty exprs in
  let new_list = Expr_set.elements s in    
    new_list


type part = Index_guard | Value_constraint | Value_constraint_index | Uncommitted
    

let get_index_set exp = 
  let rec is_int_constant exp = 
    match exp with
      | Constant (loc,c) -> true
      | Plus (loc,t1, t2) -> is_int_constant t1 && is_int_constant t2
      | Minus (loc,t1, t2) -> is_int_constant t1 && is_int_constant t2
      | Times (loc,t1, t2) -> is_int_constant t1 && is_int_constant t2
      | IDiv (loc,t1, t2) -> is_int_constant t1 && is_int_constant t2
      | Mod (loc,t1, t2) -> is_int_constant t1 && is_int_constant t2
      | UMinus (loc,t) -> is_int_constant t
      | _ -> false
  in
  let rec is_pexpr exp = 
    if is_int_constant exp then true
    else
      match exp with
        | Constant (loc,c) ->
            begin match c with
                ConstInt(_,_) -> true
              | _ -> false
            end
        | LValue (loc,l) -> 
            begin
              match l with 
                  NormLval(loc,id) -> 
                    begin
                      match quantification_of_identifier id with
                          Unquantified -> true
                        | Existential -> true
                        | Universal -> false
                    end
                | ArrayLval(_,_,_1) -> false
            end
        | Plus (loc,t1, t2) -> is_pexpr t1 && is_pexpr t2
        | Minus (loc,t1, t2) -> is_pexpr t1 && is_pexpr t2
        | Times (loc,t1, t2) -> (is_int_constant t1 && is_pexpr t2) || (is_int_constant t2 && is_pexpr t1)
        | IDiv (loc,t1, t2) -> (is_int_constant t1 && is_pexpr t2) || (is_int_constant t2 && is_pexpr t1)
        | UMinus (loc,t) -> is_pexpr t
        | ForAll (loc,decls,e) -> is_pexpr e
        | Exists (loc,decls,e) -> is_pexpr e
        | EmptyExpr -> true
        | _ -> false
  in
  let rec is_uvar exp =
    match exp with
      | LValue (loc,l) ->
          begin
            match l with 
                NormLval(loc,id) -> 
                  begin
                    match quantification_of_identifier id with
                        Unquantified -> false
                      | Existential -> false
                      | Universal -> true
                  end
              | ArrayLval(_,_,_1) -> false
          end
      | ForAll (loc,decls,e) -> is_uvar e
      | Exists (loc,decls,e) -> is_uvar e
      | _ -> false
  in
  let rec get_index_set exp part = 
    let rec get_index_set_for_disjuncts exp = 
      let exp_is_value_constraint exp = 
        try
          ignore(get_index_set exp Value_constraint);
          true
        with OutsideFragment -> false
      in
      let exp_is_index_guard exp = 
        try
          ignore(get_index_set (nnf (Not(gdl(), exp))) Index_guard);
          true
        with OutsideFragment -> false
      in        
      let rec group_disjuncts exp = 
        match exp with
            Or(loc,t1,t2) ->
              begin
                let (index_guard_t1, value_constraint_t1) = group_disjuncts t1 in
                let (index_guard_t2, value_constraint_t2) = group_disjuncts t2 in
                  (index_guard_t1 @ index_guard_t2, value_constraint_t1 @ value_constraint_t2)
              end
          | _ ->
              begin
                if exp_is_index_guard exp then ((*print_endline ("***" ^ string_of_expr exp ^ " is in index guard");*)([exp],[]))
                else if exp_is_value_constraint exp then ((*print_endline ("***" ^ string_of_expr exp ^ " is in value constraint");*)([],[exp]))
                else ((*print_endline "12";print_endline (string_of_expr exp);*) raise OutsideFragment)
              end
      in
      let (index_guard_disjuncts,value_constraint_disjuncts) = group_disjuncts exp in
      let index_set = ref [] in
      let process_value_constraint_disjunct disjunct = 
        index_set := !index_set @ get_index_set disjunct Value_constraint
      in
      let process_index_guard_disjunct disjunct = 
        index_set := !index_set @ get_index_set (nnf (Not(gdl(),disjunct))) Index_guard
      in
        List.iter process_index_guard_disjunct index_guard_disjuncts;
        List.iter process_value_constraint_disjunct value_constraint_disjuncts;
        !index_set
    in
    let process_binary_operator t1 t2 =
      if is_uvar t1 || is_uvar t2 then
        ((*print_endline "1";*)raise OutsideFragment)
      else
        match part with
            Index_guard -> assert(false)
          | Value_constraint -> get_index_set t1 part @ get_index_set t2 part
          | Value_constraint_index -> get_index_set t1 part @ get_index_set t2 part
          | Uncommitted -> get_index_set t1 part @ get_index_set t2 part
    in
    let process_unary_minus t1 = 
      if is_uvar t1 then
        ((*print_endline "2";*)raise OutsideFragment)
      else
        match part with
            Index_guard -> assert(false)
          | Value_constraint -> get_index_set t1 part
          | Value_constraint_index -> get_index_set t1 part 
          | Uncommitted -> get_index_set t1 part 
    in
    let process_eq_or_le t1 t2 = 
      match part with
          Index_guard -> 
            begin
              begin
                if is_pexpr t1 then
                  [t1]
                else if is_uvar t1 then
                  []
                else ((*print_endline "3";*)raise OutsideFragment)
              end @
              begin
                if is_pexpr t2 then
                  [t2]
                else if is_uvar t2 then
                  []
                else ((*print_endline "4";*)raise OutsideFragment)
              end;
            end
        | Value_constraint -> get_index_set t1 Value_constraint @ get_index_set t2 Value_constraint
        | Value_constraint_index -> assert(false)
        | Uncommitted -> get_index_set t1 part @ get_index_set t2 part
    in
      match exp with
      | Constant (loc,c) ->
          begin
            match part with
                Index_guard -> []
              | Value_constraint -> []
              | Value_constraint_index -> []
              | Uncommitted -> []
          end
      | LValue (loc,l) ->
          begin
            match l with
                NormLval(loc,id) ->
                  begin
                    match part with
                        Index_guard -> assert(false)
                      | Value_constraint -> 
                          begin
                            match quantification_of_identifier id with
                                Universal -> ((*print_endline "5";*)raise OutsideFragment)
                              | Existential -> ((*print_endline "6";*)raise OutsideFragment)
                              | Unquantified -> []
                          end
                      | Value_constraint_index -> []
                      | Uncommitted -> []
                  end
              | ArrayLval(loc,arr,index) ->
                  begin
                    match part with
                        Index_guard -> ((*print_endline "7";*)raise OutsideFragment)
                      | Value_constraint ->
                          begin
                            ignore (get_index_set index Value_constraint_index);
                            (if is_uvar index then [] else [index]) @ (get_index_set arr part)
                          end
                      | Value_constraint_index -> 
                          begin
                            ignore (get_index_set index Value_constraint_index);
                            ignore (get_index_set arr Value_constraint_index);
                            []
                          end
                      | Uncommitted -> (assert(not (is_uvar index));[index])
                  end
          end
      | Plus (loc,t1, t2) -> process_binary_operator t1 t2
      | Minus (loc,t1, t2) -> process_binary_operator t1 t2
      | Times (loc,t1, t2) -> process_binary_operator t1 t2
      | Div (loc,t1, t2) -> process_binary_operator t1 t2
      | IDiv (loc,t1, t2) -> process_binary_operator t1 t2
      | Mod (loc,t1, t2) -> process_binary_operator t1 t2
      | UMinus (loc,t) -> process_unary_minus t
      | ForAll (loc,decls,e) -> get_index_set_for_disjuncts e
      | Exists (loc,decls,e) -> get_index_set e part
      | ArrayUpdate (loc, exp, assign_to, assign_val) ->
          begin
            begin
              match is_pexpr assign_to with
                  true ->
                    begin
                      (get_index_set assign_val Value_constraint)
                      @ [Plus(gdl(), assign_to, Ast.Constant(gdl(),ConstInt(gdl(), 1)))]
                      @ [Minus(gdl(), assign_to, Ast.Constant(gdl(),ConstInt(gdl(), 1)))]
                    end
                | false -> ((*print_endline "8";*)raise OutsideFragment)
            end
            @ get_index_set exp part
          end
      | LT (loc,t1, t2) ->
          begin
            match part with
                Index_guard -> 
                  begin
                    if is_pexpr t1 then
                      let lhs_term = Plus(gdl(), t1, Ast.Constant(gdl(),ConstInt(gdl(), 1))) in
                        if is_pexpr t2 then
                          [lhs_term; t2]
                        else if is_uvar t2 then
                          [lhs_term]
                        else ((*print_endline "9";*)raise OutsideFragment)
                    else if is_pexpr t2 then
                      let rhs_term = Minus(gdl(), t2, Ast.Constant(gdl(),ConstInt(gdl(), 1))) in
                        if is_uvar t1 then
                          [rhs_term]
                        else ((*print_endline "10";*)raise OutsideFragment)
                    else ((*print_endline "11";*)(*print_endline (string_of_expr exp);*)raise OutsideFragment)
                  end
              | Value_constraint -> get_index_set t1 Value_constraint @ get_index_set t2 Value_constraint
              | Value_constraint_index -> assert(false)
              | Uncommitted -> get_index_set t1 part @ get_index_set t2 part
          end
      | LE (loc,t1, t2) -> process_eq_or_le t1 t2
      | GT (loc,t1, t2) -> get_index_set (LT(loc,t2,t1)) part
      | GE (loc,t1, t2) -> get_index_set (LE(loc,t2,t1)) part
      | EQ (loc,t1, t2) -> process_eq_or_le t1 t2
      | NE (loc,t1, t2) -> get_index_set (Not(gdl(), EQ(gdl(),t1,t2))) part
      | And (loc,t1, t2) -> get_index_set t1 part @ get_index_set t2 part
      | Or (loc,t1, t2) -> get_index_set t1 part @ get_index_set t2 part
      | Not (loc,t) ->
          begin
            match part with
                Index_guard ->
                  begin
                    let replacement_expr = 
                      match t with
                          LT(loc,t1,t2) -> GE(loc,t1,t2)
                        | LE(loc,t1,t2) -> GT(loc,t1,t2)
                        | GT(loc,t1,t2) -> LE(loc,t1,t2)
                        | GE(loc,t1,t2) -> LT(loc,t1,t2)
                        | EQ(loc,t1,t2) -> Or(gdl(),LT(gdl(),t1,t2),GT(gdl(),t1,t2))
                        | NE(loc,t1,t2) -> EQ(loc,t1,t2)
                        | _ -> assert(false)
                    in get_index_set replacement_expr part
                  end
              | Value_constraint -> get_index_set t part
              | Value_constraint_index -> assert(false)
              | Uncommitted -> get_index_set t part
          end
      | Iff (loc,t1, t2) -> assert(false)
      | Implies (loc,t1, t2) -> assert(false)
      | EmptyExpr -> []
      | _ -> assert(false)    
  in    
  let index_set = remove_duplicates_from_list (get_index_set (nnf exp) Uncommitted)
  in
    match List.length index_set with
        0 -> [Constant(gdl(),ConstInt(gdl(),0))]
      | _ -> index_set
          

let remove_quantification_from_vc_with_array_dp exp_orig = 

  let nnf_exp_orig = cnf exp_orig in

(*    print_endline ("before: " ^ string_of_expr exp_orig);    *)
(*    print_endline ("nnf: " ^ string_of_expr (nnf exp_orig));*)
(*    print_endline ("cnf: " ^ string_of_expr (cnf exp_orig));*)
    
    (*  let index_set = get_index_set cnf_exp_orig in*)
  let index_set = get_index_set exp_orig in
(*
  let print_exp exp = 
    print_endline (string_of_expr exp) 
  in
    print_endline "index set begin";
    List.iter print_exp index_set;
    print_endline "index set begin";
*)
  let rec rq exp = match exp with
    | Constant (loc,c) -> Constant(loc,c)
    | LValue (loc,l) -> LValue(loc,l)
    | Plus (loc,t1, t2) -> Plus(loc,rq t1, rq t2)
    | Minus (loc,t1, t2) -> Minus(loc,rq t1, rq t2)
    | Times (loc,t1, t2) -> Times(loc,rq t1, rq t2)
    | Div (loc,t1, t2) -> Div(loc,rq t1, rq t2)
    | IDiv (loc,t1, t2) -> IDiv(loc,rq t1, rq t2)
    | Mod (loc,t1, t2) -> Mod(loc,rq t1, rq t2)
    | UMinus (loc,t) -> UMinus(loc,rq t)
    | ForAll (loc,decls,e) -> 
        begin
          let remove_one_decl decl exp =
            let convert_elem_in_index_set_to_term elem = 
              let sub = [(decl.varName, elem)] in
                sub_idents_in_expr_while_preserving_original_location exp sub
            in
            let rec make_conjuncts index_set_terms_remaining expr_so_far = 
              match index_set_terms_remaining with
                  elem :: elems ->
                    let new_term = convert_elem_in_index_set_to_term elem in
                      begin
                        match expr_so_far with
                            EmptyExpr -> make_conjuncts elems new_term
                          | _ -> make_conjuncts elems (And(gdl(), expr_so_far, new_term))
                      end
                | [] -> expr_so_far
            in
              make_conjuncts index_set EmptyExpr
          in
          let rec remove_all_decls decls_remaining exp = 
            match decls_remaining with
                decl :: decls -> remove_all_decls decls (remove_one_decl decl exp)
              | [] -> exp
          in
            remove_all_decls decls e
        end
    | Exists (loc,decls,e) ->
        begin
          let changed_quant_decls_and_expr = change_quantifier decls e Unquantified in
            snd changed_quant_decls_and_expr
        end
    | ArrayUpdate (loc, exp, assign_to, assign_val) -> ArrayUpdate(loc,rq exp, rq assign_to, rq assign_val) 
    | LT (loc,t1, t2) -> LT(loc,rq t1, rq t2)
    | LE (loc,t1, t2) -> LE(loc,rq t1, rq t2)
    | GT (loc,t1, t2) -> GT(loc,rq t1, rq t2)
    | EQ (loc,t1, t2) -> EQ(loc,rq t1, rq t2)
    | NE (loc,t1, t2) -> NE(loc,rq t1, rq t2)
    | And (loc,t1, t2) -> And(loc,rq t1, rq t2)
    | Or (loc,t1, t2) -> Or(loc,rq t1, rq t2)
    | Not (loc,t) -> Not(loc,rq t)
    | Iff (loc,t1, t2) -> Iff(loc,rq t1, rq t2)
    | Implies (loc,t1, t2) -> Implies(loc, rq t1, rq t2)
    | _ -> exp
  in
    rq (nnf_exp_orig) ;;

let rec get_calls e =
  let get_calls_lval l = match l with
    | NormLval (_, _) -> []
    | ArrayLval (l, e1, e2) -> get_calls e1 @ get_calls e2
  in
  match e with
  | Assign (loc,l, e) -> get_calls_lval l @ get_calls e
  | Constant (loc,c) -> []
  | LValue (loc,l) -> get_calls_lval l
  | Call (loc,s, el) ->
      let arg_results = List.rev_map get_calls el in
      e :: List.concat arg_results
  | Plus (loc,t1, t2) -> get_calls t1 @ get_calls t2
  | Minus (loc,t1, t2) -> get_calls t1 @ get_calls t2
  | Times (loc,t1, t2) -> get_calls t1 @ get_calls t2
  | Div (loc,t1, t2) -> get_calls t1 @ get_calls t2
  | IDiv (loc,t1, t2) -> get_calls t1 @ get_calls t2
  | Mod (loc,t1, t2) -> get_calls t1 @ get_calls t2
  | UMinus (loc,t) -> get_calls t
  | ForAll (loc,decls,e) -> get_calls e
  | Exists (loc,decls,e) -> get_calls e
  | ArrayUpdate (loc,e,assign_to,assign_val) -> get_calls e @ get_calls assign_to @ get_calls assign_val
  | LT (loc,t1, t2) -> get_calls t1 @ get_calls t2
  | LE (loc,t1, t2) -> get_calls t1 @ get_calls t2
  | GT (loc,t1, t2) -> get_calls t1 @ get_calls t2
  | GE (loc,t1, t2) -> get_calls t1 @ get_calls t2
  | EQ (loc,t1, t2) -> get_calls t1 @ get_calls t2
  | NE (loc,t1, t2) -> get_calls t1 @ get_calls t2
  | And (loc,t1, t2) -> get_calls t1 @ get_calls t2
  | Or (loc,t1, t2) -> get_calls t1 @ get_calls t2
  | Not (loc,t) -> get_calls t
  | Iff (loc,t1, t2) -> get_calls t1 @ get_calls t2
  | Implies (loc,t1, t2) -> get_calls t1 @ get_calls t2
  | Length (loc, t) -> get_calls t
  | NewArray (loc, t, e) -> get_calls e
  | EmptyExpr  -> [] ;;


