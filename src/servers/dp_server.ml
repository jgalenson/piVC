open Server_framework ;;
open Net_utils ;;

exception DP_Server_Timeout ;;

(* Extract if context id is currently sat.  Parse through
 * annoying messages.
 *)
let rec getresponse id =
  (* Check to make sure we don't put \n at end of returned string. *)
  let append a b =
    if b = "" then a else (a ^ "\n" ^ b)
  in
  try
    (* Gets the counterexample by reading until we hit a newline. *)
    let rec get_counterexample () = match (Ci_yices.recv id) with
	| "" -> ""
	| x -> append x (get_counterexample ())
    in
    (* Get (response, counterexample opt) from yices. *)
    let recv = Ci_yices.recv id in
      match recv with
	| "sat" -> ("sat", Some (get_counterexample ()))
        | "unknown" -> ("unknown", None)
	| "unsat" -> ("unsat", None)
	| "Logical context is inconsistent. Use (pop) to restore the previous state." -> assert false
	| "ok" -> getresponse id
	| "" -> ("", None)
	| _ -> ("", None)
  with
    | End_of_file ->
	assert false ;;

let verify ic oc =
  (*print_endline "dp server begin";*)
  begin
    try
      ignore (Sys.set_signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise DP_Server_Timeout)));
      ignore (Unix.alarm (Config.get_value_int "timeout_time"));
      let input = get_input ic in
      Config.print ("dp got input: " ^ input); (* input contains its own endline. *)
      Ci_yices.init ();
      let id = Ci_yices.new_context () in
      Ci_yices.send id input;
      begin
	try
	  Ci_yices.wait id
	with
	    ex ->
	      Ci_yices.kill id;
	      raise ex
      end;
      let (response, counterexample_opt) = getresponse id in
      Ci_yices.delete_context id;
      assert ((response = "sat") = (Utils.is_some counterexample_opt));
      let response_str = response ^ (if (Utils.is_some counterexample_opt) then (" with " ^ (Utils.elem_from_opt counterexample_opt)) else "") in
      Config.print ("Got response: " ^ response_str);
      send_output oc response;
      if (response = "sat") then
        send_output oc (Utils.elem_from_opt counterexample_opt)
    with
      | Ci_yices.NonLinearProblem ->
	  send_output oc "non-linear"
      | ex ->
	  begin
	    let error_str = Exceptions.string_of_exception ex in
            send_output oc "error";
            send_output oc error_str
	  end 
  end;
  (*print_endline "dp server end";*)
  Sys.set_signal Sys.sigalrm Sys.Signal_ignore;
  flush oc ;;

let start_dp_server () =
  Config.set_server_type Config.DPServer;
  Config.load (Utils.get_absolute_path Constants.dp_server_config_file_path);
  run_server verify (Config.get_value_int "port") ;;
