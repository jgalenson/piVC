open Server_framework ;;
open Unix ;;
open Printf ;;

let _ = 
  Config.parse_cmd_line Config.BothServers;
  match Config.get_cmd_line_value_iff_key_exists "test_email_addr" with
      Some(v) -> Email.send_test_email ()
    | None ->
        begin
          match Unix.fork () with
            | 0 ->
                Main_server.start_main_server ()
            | _ ->
	        Dp_server.start_dp_server ()
        end
