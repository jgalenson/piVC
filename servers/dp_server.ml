open Server_framework ;;
open Net_utils ;;   

(* Extract if context id is currently sat.  Parse through
 * annoying messages.
 *)
let rec getsat id =
  try
    Ci_yices.wait id;
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
  print_endline "Beginning verify.";
  let input = get_input ic in
  print_endline ("dp got input: " ^ input);
  Ci_yices.init ();
  let id = Ci_yices.new_context () in
  print_endline "Sending to yices.";
  Ci_yices.send id input;
  print_endline "Getting response.";
  let is_sat = getsat id in
  print_endline ("Got response: " ^ is_sat);
  send_output oc is_sat;
  flush oc;
  print_endline "End verify.";
