open Server_framework ;;
open Net_utils ;;   

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
	assert false ;;

let verify ic oc =
  print_string "Beginning verify.";
  let input = get_input ic in
  Ci_yices.init ();
  let id = Ci_yices.new_context () in
  Ci_yices.send id input;
  let is_sat = getsat id in
  send_output oc is_sat;
  flush oc;
  print_string "End verify.";
