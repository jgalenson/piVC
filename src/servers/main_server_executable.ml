open Server_framework ;;
open Main_server ;;

let _ =
  Config.parse_cmd_line Config.MainServer;
  match Config.get_cmd_line_value_iff_key_exists "test_email_addr" with
      Some(v) -> Email.send_test_email ()
    | None -> Main_server.start_main_server ()
