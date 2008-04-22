open Ast

exception CantReplaceLValueWithExpr
exception NotLValueExpr
exception InvalidFormula


(* CODE SECTION: SUBSTITUTING VARIABLE NAMES IN EXPRS *)


let rec array_name_from_lval lval = match lval with
    NormLval(loc,id) -> Some(id.name)
  | ArrayLval(loc,arr,index) -> 
      begin
        match arr with
            LValue(loc,lval) -> array_name_from_lval lval
          | _ -> None (*If, for example, the user is using an index from a function call, there is no array name*)
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
    | _ -> EmptyExpr (*Nothing can be assigned to a non-lval, so even if the user has used an assignment operator, there's no real assignment taking place, and hence we ignore it.*)
(*TODO-A: is EmptyExpr doing the correct thing? test*)

let get_match_sub lval_str ident_subs = 
  let sub_to_return = ref None in
  let replacement_func possibility = match (String.compare lval_str (fst possibility)) with
      0 -> sub_to_return := Some(snd possibility)
    | _ -> ignore ()
  in
    List.iter replacement_func ident_subs;
    !sub_to_return

(* Substitutes variable names, but does not substitue function/predicate names.
   Also, if it runs across var decls due to a quantifier, it won't replace those new identifiers*)
let rec sub_idents_in_expr expr ident_subs = 
  let rec get_new_ident_subs old_ident_subs decls = 
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
                    let match_sub = get_match_sub id.name ident_subs in
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
                    let match_sub = get_match_sub id.name ident_subs in
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
      | ForAll (loc,decls,e) -> ForAll(loc,decls,sub_idents_in_expr e (get_new_ident_subs ident_subs decls))
      | Exists (loc,decls,e) -> Exists(loc,decls,sub_idents_in_expr e (get_new_ident_subs ident_subs decls))
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

(* returns a list of the identifier names of the function's arguments *)
let get_idents_of_formals func = 
  let rec build_ident_list remaining = 
    match remaining with
        [] -> []
      | e :: l -> e.varName.name :: build_ident_list l
  in 
    build_ident_list func.formals



let change_quantifier old_decls old_expr new_quant =
  let old_decl_to_new_decl decl = 
    {varType=decl.varType; varName=decl.varName; location_vd=decl.location_vd; var_id = decl.var_id; (*TODO-A: may need to re-number here*) quant = new_quant}
  in
  let new_decls = List.map old_decl_to_new_decl old_decls in
  let new_decl_to_replacement_pair new_decl = 
    (new_decl.varName.name,LValue(gdl(),NormLval(gdl(),{name = new_decl.varName.name; location_id = gdl(); decl = ref (Some(new_decl)); is_length = false;})))
  in
  let replacement_pairs = List.map new_decl_to_replacement_pair new_decls in
  let new_expr = sub_idents_in_expr old_expr replacement_pairs in
    (new_decls,new_expr)
    

let rec nnf expr = 
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

let get_index_set exp = 
  let contains_universal_quantification exp =
    let rec cuq exp = 
      match exp with
        | Constant (loc,c) -> false
        | LValue (loc,l) ->
            begin
              match l with
                  NormLval(loc,id) -> ((varDecl_of_identifier id).quant == Universal)
                | ArrayLval(loc,arr,index) -> false
            end
        | Plus (loc,t1, t2) -> cuq t1 or cuq t2
        | Minus (loc,t1, t2) -> cuq t1 or cuq t2
        | Times (loc,t1, t2) -> cuq t1 or cuq t2
        | Div (loc,t1, t2) -> cuq t1 or cuq t2
        | IDiv (loc,t1, t2) -> cuq t1 or cuq t2
        | Mod (loc,t1, t2) -> cuq t1 or cuq t2
        | UMinus (loc,t) -> cuq t
        | ForAll (loc,decls,e) -> cuq e
        | Exists (loc,decls,e) ->  cuq e
        | ArrayUpdate (loc, exp, assign_to, assign_val) -> cuq assign_to or cuq assign_val
        | LT (loc,t1, t2) -> cuq t1 or cuq t2
        | LE (loc,t1, t2) -> cuq t1 or cuq t2
        | GT (loc,t1, t2) -> cuq t1 or cuq t2
        | GE (loc,t1, t2) -> cuq t1 or cuq t2
        | EQ (loc,t1, t2) -> cuq t1 or cuq t2
        | NE (loc,t1, t2) -> cuq t1 or cuq t2
        | And (loc,t1, t2) -> cuq t1 or cuq t2
        | Or (loc,t1, t2) -> cuq t1 or cuq t2
        | Not (loc,t) -> cuq t
        | Iff (loc,t1, t2) -> cuq t1 or cuq t2
        | Implies (loc,t1, t2) -> cuq t1 or cuq t2
        | EmptyExpr -> false
        | _ -> raise InvalidFormula
    in
      cuq exp
  in
  let get_array_indices exp = 
    let rec gai exp = 
      match exp with
        | Constant (loc,c) -> []
        | LValue (loc,l) ->
            begin
              match l with 
                  ArrayLval(loc,arr,index) -> 
                    begin
                      let inside = gai arr in
                        if (contains_universal_quantification index) then inside else inside @ [index]
                    end
                | _ -> []
            end
        | Plus (loc,t1, t2) -> gai t1 @ gai t2
        | Minus (loc,t1, t2) -> gai t1 @ gai t2
        | Times (loc,t1, t2) -> gai t1 @ gai t2
        | Div (loc,t1, t2) -> gai t1 @ gai t2
        | IDiv (loc,t1, t2) -> gai t1 @ gai t2
        | Mod (loc,t1, t2) -> gai t1 @ gai t2
        | UMinus (loc,t) -> gai t
        | ForAll (loc,decls,e) -> gai e
        | Exists (loc,decls,e) -> gai e
        | ArrayUpdate (loc, exp, assign_to, assign_val) -> if (contains_universal_quantification assign_to) then [] else [assign_to]
        | LT (loc,t1, t2) -> gai t1 @ gai t2
        | LE (loc,t1, t2) -> gai t1 @ gai t2
        | GT (loc,t1, t2) -> gai t1 @ gai t2
        | GE (loc,t1, t2) -> gai t1 @ gai t2
        | EQ (loc,t1, t2) -> gai t1 @ gai t2
        | NE (loc,t1, t2) -> gai t1 @ gai t2
        | And (loc,t1, t2) -> gai t1 @ gai t2
        | Or (loc,t1, t2) -> gai t1 @ gai t2
        | Not (loc,t) -> gai t
        | Iff (loc,t1, t2) -> gai t1 @ gai t2
        | Implies (loc,t1, t2) -> gai t1 @ gai t2
        | EmptyExpr -> []
        | _ -> raise InvalidFormula
    in gai exp
  in(*
  let get_guards exp  = 
    (*Returns list with single element of exp if exp is not universally quantified.
      Otherwise, returns an empty list.*)
    let gl exp = 
      if contains_universal_quantification exp then [] else [exp]
    in
    let rec gg exp = 
      match exp with
        | Constant (loc,c) -> []
        | LValue (loc,l) -> []
        | Plus (loc,t1, t2) -> []
        | Minus (loc,t1, t2) -> []
        | Times (loc,t1, t2) -> []
        | Div (loc,t1, t2) -> []
        | IDiv (loc,t1, t2) -> []
        | Mod (loc,t1, t2) -> []
        | UMinus (loc,t) -> []
        | ForAll (loc,decls,e) -> gg e
        | Exists (loc,decls,e) -> gg e
        | ArrayUpdate (loc, exp, assign_to, assign_val) -> []
        | LT (loc,t1, t2) -> gl t1 @ gl t2
        | LE (loc,t1, t2) -> gl t1 @ gl t2
        | GT (loc,t1, t2) -> gl t1 @ gl t2
        | GE (loc,t1, t2) -> gl t1 @ gl t2
        | EQ (loc,t1, t2) -> gl t1 @ gl t2
        | NE (loc,t1, t2) -> gl t1 @ gl t2
        | And (loc,t1, t2) -> gg t1 @ gg t2
        | Or (loc,t1, t2) -> gg t1 @ gg t2
        | Not (loc,t) -> gg t
        | Iff (loc,t1, t2) -> gg t1 @ gg t2
        | Implies (loc,t1, t2) -> gg t1 @ gg t2
        | EmptyExpr -> []
        | _ -> raise InvalidFormula
    in
      gg exp
  in*)
    remove_duplicates_from_list (get_array_indices exp (*@ get_guards exp*) @ [Constant(gdl(),ConstInt(gdl(),0))])
    (*We always include 0 in the index set. Technically, 0 needs only be included if the index
      set would be otherwise empty. However, because we don't always have the formula in the
      simple implication form used by the DP, we use heuristics to decide what to put in the
      index set. Our index will always be a superset of what it needs to be, so it's never
      a problem. It is possible that the index set should only contain the 0 element, but
      because our index set is over-sized, we do not realize it would be empty (apart from
      the 0 element), and hence we might not include the 0 element. To prevent this from
      happening, we always include the 0 element, regardless of whether or not it needs
      to be there.*)
      
let remove_quantification_from_vc_with_array_dp exp_orig = 
  let nnf_exp_orig = nnf exp_orig in
  let index_set = get_index_set nnf_exp_orig in
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
              let sub = [(decl.varName.name, elem)] in
                sub_idents_in_expr exp sub
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
    rq (nnf_exp_orig)
      
      
      

