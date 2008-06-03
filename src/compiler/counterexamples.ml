open Ast ;;
open Scanner ;;

exception StrangeCounterexample of string ;;

type variable =
  | Var of string * identifier
  | ArrayVar of variable * string
  | Div of string * string ;;

type example = variable * string ;;

let rec variable_to_string v =
  match v with
    | Var (s,_) -> s
    | ArrayVar (v, s) -> (variable_to_string v) ^ "[" ^ s  ^ "]"
    | Div (num, denom) -> num ^ " div " ^ denom ;;

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

let parse_counterexample str rev_var_names =
  (* print_endline str; *)
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
    if (first_token <> "(") then
      if first_token = "div" then begin (* yices puts in some stupid (div 1 2) nodes when you do integer division. *)
	let num = Scanner.next_token scan in
	let denom = Scanner.next_token scan in
	ignore (Scanner.next_token scan); (* Closing ")" *)
	Div (num, denom)
      end else
	Var (first_token, Hashtbl.find rev_var_names orig_first_token)
    else
      begin
	let var = parse_counterexample scan in
	match var with
	  | Div (_, _) -> var
	  | _ ->
	      let rhs = get_replaced_token scan in
	      let array_var = ArrayVar (var, rhs) in
	      ignore (Scanner.next_token scan); (* Closing ")" *)
	      array_var
      end
  in
      
  let parts = Str.split (Str.regexp "\n") str in
  let map_fn s =
    let scan = Scanner.create s in
    assert (Scanner.next_token scan = "(");
    assert (Scanner.next_token scan = "=");
    let lhs = parse_counterexample scan in
    let rhs = get_replaced_token scan in
    (lhs, rhs)
  in
  let data = List.map map_fn parts in
  let sort_fn (lhs1, _) (lhs2, _) =
    String.compare (variable_to_string lhs1) (variable_to_string lhs2)
  in
  let sorted_data = List.sort sort_fn data in
  (* print_endline ("Counterexample: " ^ (counterexample_to_string sorted_data)); *)
  sorted_data ;;
