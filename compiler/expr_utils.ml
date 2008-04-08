open Ast

exception CantReplaceLValueWithExpr
exception NotLValueExpr
exception InvalidFormula



let rec nnf expr = 
  match expr with
    | Assign (loc,l, e) -> raise InvalidFormula
    | Constant (loc,c) -> expr
    | LValue (loc,l) -> expr
    | Call (loc,s, el) -> raise InvalidFormula
    | Plus (loc,t1, t2) -> Plus(loc, nnf t1, nnf t2)
    | Minus (loc,t1, t2) -> Minus(loc, nnf t1, nnf t2)
    | Times (loc,t1, t2) -> Times(loc, nnf t1, nnf t2)
    | Div (loc,t1, t2) -> Div(loc, nnf t1, nnf t2)
    | IDiv (loc,t1, t2) -> IDiv(loc, nnf t1, nnf t2)
    | Mod (loc,t1, t2) -> Mod(loc, nnf t1, nnf t2)
    | UMinus (loc,t) -> UMinus(loc, nnf t)
    | ForAll (loc,decls,e) -> ForAll(loc,decls,nnf e)
    | Exists (loc,decls,e) -> Exists(loc,decls,nnf e)
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
              | ForAll(loc2,decls,e) -> Exists(loc2,decls,nnf (Not(loc2,nnf e)))
              | Exists(loc2,decls,e) -> ForAll(loc2,decls,nnf (Not(loc2,nnf e)))
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



(* CODE SECTION: SUBSTITUTING VARIABLE NAMES IN EXPRS *)

(*
let ident_name_of_lval lval = match lval with
    NormLval(loc, id) -> id.name
  | ArrayLval(loc, id, expr) -> id.name
*)


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
(*
  let replace_lval_ident lval new_ident = 
    match lval with
        NormLval(loc,s) -> NormLval(loc,new_ident)
      | ArrayLval(loc,old_ident,index) -> (print_string (string_of_lval (ArrayLval(loc,new_ident,index)));ArrayLval(loc,new_ident,index))
  in
  let sub_idents_in_lval lval =
    match get_match_sub lval ident_subs with
      None -> lval
    | Some(sub) -> (
        match snd sub with
            LValue(loc, l) -> replace_lval_ident lval (identifier_of_lval l)
          | _ -> (raise (CantReplaceLValueWithExpr))
      )
  in
  (*TODO-A: If we allow array reads on abritrary exprs, this function will
    need to be updated *)
  let sub_idents_in_lval_expr lval_expr =
    match lval_expr with
      LValue(loc, l) ->
        begin
          match get_match_sub l ident_subs with
              None -> lval_expr
            | Some(sub) ->
                begin
                  match snd sub with
                      LValue(loc_sub,l_sub) -> LValue(loc, replace_lval_ident l (identifier_of_lval l_sub))
                    | _ -> snd sub
                end
        end
    | _ -> (raise NotLValueExpr)
  in
*)
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
