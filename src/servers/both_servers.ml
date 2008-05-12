open Server_framework ;;
open Unix ;;
open Printf ;;

let _ = 
  match Unix.fork () with
    | 0 ->
        Main_server.start_main_server ()
    | _ ->
	Dp_server.start_dp_server () ;;

