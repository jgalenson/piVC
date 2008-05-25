open Server_framework ;;
open Unix ;;
open Printf ;;

let _ = 
  Config.parse_cmd_line Config.BothServers;
  match Unix.fork () with
    | 0 ->
        Main_server.start_main_server ()
    | _ ->
	Dp_server.start_dp_server () ;;

