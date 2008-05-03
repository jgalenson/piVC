open Server_framework ;;
open Main_server ;;

let _ =
  Config.load (Utils.get_absolute_path Constants.main_server_config_file_path);
  run_server (get_main_server_func ()) (*(Config.get_value_int "port")*)(*Constants.main_server_port*)4242 ;;
