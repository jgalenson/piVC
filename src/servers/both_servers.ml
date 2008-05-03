open Server_framework ;;
open Unix ;;
open Printf ;;

let _ = 
  match Unix.fork () with
    | 0 ->
        begin
          Config.load (Utils.get_absolute_path Constants.main_server_config_file_path);          
          run_server (Main_server.get_main_server_func ()) (Config.get_value_int "port")
        end
    | _ ->
        begin
          Config.load (Utils.get_absolute_path Constants.dp_server_config_file_path);          
          run_server Dp_server.verify (Config.get_value_int "port")
        end
;;

