(*Jason's really simple test*)
(*let _ =
  Ci_yices.init ();
  let context = Ci_yices.new_context() in
    Ci_yices.send context "(assert (/= 1 1))(check)";
    Ci_yices.wait context;
    let result = Ci_yices.recv context in
      print_string("result: " ^ result ^ "\n")
*)     
    
(* Extract if context id is currently sat.  Parse through
 * annoying messages.
 *)
let rec getsat id =
  try
    print_endline "a";
    Ci_yices.wait id;
    print_endline "b";
    let recv = Ci_yices.recv id in
      match recv with
	| "sat" -> "sat"
	| "unsat" -> "unsat"
	| "Logical context is inconsistent. Use (pop) to restore the previous state." 
	  -> getsat id
	| "ok" -> getsat id
	| x ->
	    print_endline x;
	    getsat id
  with
    | End_of_file ->
	assert false

(* Get if context id is satisfiable. *)
let sat id = 
  Ci_yices.send id "(check)";
  getsat id

let test_yices expr_str =
  Ci_yices.init ();
  let id = Ci_yices.new_context () in
  Ci_yices.send id "(define x::bool)\n(assert x)\n(check)\n" ;
  let is_sat = getsat id in
  print_endline ("result: " ^ is_sat) ;;

let _ =
  test_yices "x" ;;


