open Server_framework ;;
open Main_server ;;

let _ = run_server (get_main_server_func ()) Constants.main_server_port ;;
