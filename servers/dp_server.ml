open Server_framework ;;
open Net_utils ;;   

(* Extract if context id is currently sat.  Parse through
 * annoying messages.
 *)
let rec getresponse id =
  (* Check to make sure we don't put \n at end of returned string. *)
  let append a b =
    if b = "" then a else (a ^ "\n" ^ b)
  in
  try
    let recv = Ci_yices.recv id in
      match recv with
	| "sat" -> append "sat" (getresponse id)
        | "unknown" -> "unknown"
	| "unsat" -> "unsat"
	| "Logical context is inconsistent. Use (pop) to restore the previous state." 
	  -> assert false
	| "ok" -> getresponse id
	| "" -> ""
	| x -> append x (getresponse id)
  with
    | End_of_file ->
	assert false ;;

let verify ic oc =
  let input = get_input ic in
  print_endline ("dp got input: " ^ Utils.truncate_for_printing input); (* input contains its own endline. *)
  Ci_yices.init ();
  let id = Ci_yices.new_context () in
  Ci_yices.send id input;
  Ci_yices.wait id;
  let is_sat = getresponse id in
  Ci_yices.delete_context id;
  print_endline ("Got response: " ^ is_sat);
  send_output oc is_sat;
  flush oc;
