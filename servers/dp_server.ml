open Server_framework ;;
open Net_utils ;;   

(* Extract if context id is currently sat.  Parse through
 * annoying messages.
 *)
let rec getresponse id =
  try
    let recv = Ci_yices.recv id in
      match recv with
	| "sat" -> "sat\n" ^ (getresponse id)
        | "unknown" -> "unknown"
	| "unsat" -> "unsat"
	| "Logical context is inconsistent. Use (pop) to restore the previous state." 
	  -> getresponse id
	| "ok" -> getresponse id
	| "" -> ""
	| x ->
	    x ^ "\n" ^ (getresponse id)
  with
    | End_of_file ->
	assert false ;;

let verify ic oc =
  let input = get_input ic in
  print_endline ("dp got input: " ^ input);
  Ci_yices.init ();
  let id = Ci_yices.new_context () in
  Ci_yices.send id input;
  Ci_yices.wait id;
  let is_sat = getresponse id in
  print_endline ("Got response: " ^ is_sat);
  send_output oc is_sat;
  flush oc;
