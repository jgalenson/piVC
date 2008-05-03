open Server_framework ;;
open Dp_server ;;

let _ = 
  Config.load (Utils.get_absolute_path Constants.dp_server_config_file_path);
  run_server verify (Config.get_value_int "port") ;;
