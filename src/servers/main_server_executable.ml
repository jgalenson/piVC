open Server_framework ;;
open Main_server ;;

let _ =
  Config.parse_cmd_line Config.MainServer;
  Main_server.start_main_server () ;;
