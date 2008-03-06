open Server_framework ;;

let dp_server ic oc =
  let input = get_input ic in
  send_output oc input;
  flush oc

let _ = run_server dp_server ;;
