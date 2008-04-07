open Ast ;;
open Scanner ;;

exception StrangeCounterexample of string ;;

type variable =
  | Var of string
  | ArrayVar of variable * string ;;

type example = variable * string ;;

let rec variable_to_string v =
  match v with
    | Var (s) -> s
    | ArrayVar (v, s) -> (variable_to_string v) ^ "[" ^ s  ^ "]";;

let counterexample_to_string cx =
  let example_to_string prev (lhs, rhs) =
    let new_part = (variable_to_string lhs) ^ "=" ^ rhs in 
    if prev = "" then new_part else prev ^ "\n" ^ new_part
  in
  List.fold_left example_to_string "" cx ;;

let parse_counterexamples str rev_var_names =
  
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
    let first_token = get_replaced_token scan in
    if (first_token <> "(") then
      Var (first_token)
    else
      begin
	let var = parse_counterexample scan in
	let rhs = get_replaced_token scan in
	let array_var = ArrayVar (var, rhs) in
	ignore (Scanner.next_token scan); (* Closing ")" *)
	array_var
      end
  in
      
  let parts = List.tl (Str.split (Str.regexp "\n") str) in (* Ignore first part ("sat"). *)
  let map_fn s =
    print_endline ("Scanning: " ^ s);
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
  let sorted_data = List.sort sort_fn data in (* Do we need this? *)
  print_endline ("Counterexample: " ^ (counterexample_to_string sorted_data));
  sorted_data ;;
