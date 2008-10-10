open Server_framework ;;
open Net_utils ;;

exception DP_Server_Timeout ;;

let verify ic oc =
  (*print_endline "dp server begin";*)
  begin
    try
      ignore (Sys.set_signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise DP_Server_Timeout)));
      ignore (Unix.alarm (Config.get_value_int "timeout_time"));
      let input = get_input ic in
      Config.print ("dp got input: " ^ input); (* input contains its own endline. *)
      let (response, counterexample_opt) = Smt_solver_interface.get_response_from_smt_solver input in
      assert ((response = "sat") = (Utils.is_some counterexample_opt));
      let response_str = response ^ (if (Utils.is_some counterexample_opt) then (" with " ^ (Utils.elem_from_opt counterexample_opt)) else "") in
      Config.print ("Got response: " ^ response_str);
      send_output oc response;
      if (response = "sat") then
        send_output oc (Utils.elem_from_opt counterexample_opt)
    with
      | Smt_solver_interface.NonLinearProblem ->
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
