open Server_framework ;;

let _ =
  match Unix.fork () with
    | 0 -> run_server Main_server.compile Constants.main_server_port ;
    | _ -> run_server Dp_server.verify Constants.dp_server_port ;;
