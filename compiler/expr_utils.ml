open Ast

exception CantReplaceLValueWithExpr
exception NotLValueExpr

(* CODE SECTION: SUBSTITUTING VARIABLE NAMES IN EXPRS *)

let ident_name_of_lval lval = match lval with
    NormLval(loc, id) -> id.name
  | ArrayLval(loc, id, expr) -> id.name

let get_match_sub lval ident_subs = 
  let sub_to_return = ref None in
  let replacement_func possibility = match (String.compare (ident_name_of_lval lval) (fst possibility)) with
      0 -> sub_to_return := Some(possibility)
    | _ -> ignore ()
  in
    List.iter replacement_func ident_subs;
    !sub_to_return

let sub_idents_in_lval lval ident_subs = match get_match_sub lval ident_subs with
    None -> lval
  | Some(sub) -> (
      match snd sub with
          LValue(loc, l) -> l
        | _ -> (raise (CantReplaceLValueWithExpr))
    )

let sub_idents_in_lval_expr lval_expr ident_subs = match lval_expr with
  LValue(loc, l) -> (
    match get_match_sub l ident_subs with
        None -> lval_expr
      | Some(sub) -> snd sub
  )
  | _ -> (raise NotLValueExpr)


(* substitutes variable names, but does not substitue function/predicate names *)
let rec sub_idents_in_expr expr ident_subs = 
    match expr with
    | Assign (loc,l, e) -> Assign(loc, sub_idents_in_lval l ident_subs, sub_idents_in_expr e ident_subs)
    | Constant (loc,c) -> expr
    | LValue (loc,l) -> sub_idents_in_lval_expr expr ident_subs
    | Call (loc,s, el) -> Call(loc, s, sub_idents_in_expr_list el ident_subs)
    | Plus (loc,t1, t2) -> Plus(loc, sub_idents_in_expr t1 ident_subs, sub_idents_in_expr t2 ident_subs)
    | Minus (loc,t1, t2) -> Minus(loc, sub_idents_in_expr t1 ident_subs, sub_idents_in_expr t2 ident_subs)
    | Times (loc,t1, t2) -> Times(loc, sub_idents_in_expr t1 ident_subs, sub_idents_in_expr t2 ident_subs)
    | Div (loc,t1, t2) -> Div(loc, sub_idents_in_expr t1 ident_subs, sub_idents_in_expr t2 ident_subs)
    | IDiv (loc,t1, t2) -> IDiv(loc, sub_idents_in_expr t1 ident_subs, sub_idents_in_expr t2 ident_subs)
    | Mod (loc,t1, t2) -> Mod(loc, sub_idents_in_expr t1 ident_subs, sub_idents_in_expr t2 ident_subs)
    | UMinus (loc,t) -> UMinus(loc, sub_idents_in_expr t ident_subs)
    | LT (loc,t1, t2) -> LT(loc, sub_idents_in_expr t1 ident_subs, sub_idents_in_expr t2 ident_subs)
    | LE (loc,t1, t2) -> LE(loc, sub_idents_in_expr t1 ident_subs, sub_idents_in_expr t2 ident_subs)
    | GT (loc,t1, t2) -> GT(loc, sub_idents_in_expr t1 ident_subs, sub_idents_in_expr t2 ident_subs)
    | GE (loc,t1, t2) -> GE(loc, sub_idents_in_expr t1 ident_subs, sub_idents_in_expr t2 ident_subs)
    | EQ (loc,t1, t2) -> EQ(loc, sub_idents_in_expr t1 ident_subs, sub_idents_in_expr t2 ident_subs)
    | NE (loc,t1, t2) -> NE(loc, sub_idents_in_expr t1 ident_subs, sub_idents_in_expr t2 ident_subs)
    | And (loc,t1, t2) -> And(loc, sub_idents_in_expr t1 ident_subs, sub_idents_in_expr t2 ident_subs)
    | Or (loc,t1, t2) -> Or(loc, sub_idents_in_expr t1 ident_subs, sub_idents_in_expr t2 ident_subs)
    | Not (loc,t) -> Not(loc, sub_idents_in_expr t ident_subs)
    | Iff (loc,t1, t2) -> Iff(loc, sub_idents_in_expr t1 ident_subs, sub_idents_in_expr t2 ident_subs)
    | Implies (loc,t1, t2) -> Implies(loc, sub_idents_in_expr t1 ident_subs, sub_idents_in_expr t2 ident_subs)
    | Length (loc, t) -> Length(loc, sub_idents_in_expr t ident_subs)
    | EmptyExpr  -> expr

and sub_idents_in_expr_list expr_list ident_subs = 
  match expr_list with 
      [] -> []
    | e::l -> sub_idents_in_expr e ident_subs :: sub_idents_in_expr_list l ident_subs


(* returns a list of the identifier names of the function's arguments *)
let get_idents_of_formals func = 
  let rec build_ident_list remaining = 
    match remaining with
        [] -> []
      | e :: l -> e.varName.name :: build_ident_list l
  in 
    build_ident_list func.formals
