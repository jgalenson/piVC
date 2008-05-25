open Server_framework ;;
open Dp_server ;;

let _ = 
  Config.parse_cmd_line Config.DPServer;
  Dp_server.start_dp_server () ;;
