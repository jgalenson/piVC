module String_map = Map.Make(String)

exception Config_Key_Not_Found of string;;
exception Cmd_Line_Arg_Key_Not_Found of string;;
exception Config_Key_Could_Not_Be_Parsed_To_Int of string * string;;
exception Config_Key_Could_Not_Be_Parsed_To_Bool of string * string;;

let key_value_map = ref String_map.empty

let get_value key = 
  try
    String_map.find key (!key_value_map)
  with
      Not_found -> raise (Config_Key_Not_Found key)
        
let get_value_int key = 
  let value = get_value key in
  try
    int_of_string (value)
  with
      Failure("int_of_string") -> raise (Config_Key_Could_Not_Be_Parsed_To_Int (key,value))
        
let get_value_bool key = 
  let value = get_value key in
  try
    bool_of_string (value)
  with
      Failure("int_of_string") -> raise (Config_Key_Could_Not_Be_Parsed_To_Bool (key,value))

let read_from_file file_path = 
  let file = Unix.openfile file_path [Unix.O_RDONLY] 0o640 in
  let file_size = (Unix.fstat file).Unix.st_size in
  let file_text =
    let file_text_temp = String.create file_size in
    ignore (Unix.read file file_text_temp 0 file_size);
    file_text_temp
  in
  let rec load_key_values str = 
    if String.length str > 0 then
      let index_of_end =
        if String.contains str '\n' then
          String.index str '\n'
        else
          String.length str
      in
      let key_value = (String.sub str 0 index_of_end) in
      let rest = 
        if index_of_end < (String.length str)-1 then
          String.sub str (index_of_end + 1) ((String.length str)-index_of_end-1)
        else
          ""
      in
        if (String.length key_value > 0) && (String.get key_value 0 != '#') then
          begin
            let index_of_equals = String.index key_value '=' in
            let key = Utils.trim (String.sub key_value 0 index_of_equals) in 
            let value = Utils.trim (String.sub key_value (index_of_equals + 1) ((String.length key_value)-index_of_equals-1))
            in
              String_map.add key value (load_key_values rest)
          end
        else
          load_key_values rest
    else
      String_map.empty
  in
    load_key_values file_text ;;

(* Command-line argument support. *)

(* Let us know what server is currently running. *)

type server_type =
  | MainServer
  | DPServer
  | BothServers
  | Parser ;;

(* Global to store the currently-running server-type. *)
let server_type = ref None ;;

(* Code to parse command-line arguments. *)

let cmd_line_arg_map = ref String_map.empty ;;

let add_to_map key value () =
  cmd_line_arg_map := String_map.add key value !cmd_line_arg_map ;;

let add_to_map_no_dummy key value =
  cmd_line_arg_map := String_map.add key value !cmd_line_arg_map ;;

(* The command-line arguments we accept for each type of server. *)
let both_servers_args = [
  "--print-main-server-info",Arg.Unit (add_to_map "print_main_server_info" "true"),"Print debug info in main server.";
  "--no-print-main-server-info", Arg.Unit (add_to_map "print_main_server_info" "false"), "Do not print debug info in main server.";
  "--print-dp-server-info", Arg.Unit (add_to_map "print_dp_server_info" "true"), "Print debug info in dp server.";
  "--no-print-dp-server-info", Arg.Unit (add_to_map "print_dp_server_info" "false"), "Do not print debug info in dp server.";
  "--print-net-msgs", Arg.Unit (add_to_map "print_net_msgs" "true"), "Print messages for network events.";
  "--no-print-net-msgs", Arg.Unit (add_to_map "print_net_msgs" "false"), "Do not print messages for network events.";
  "--truncate-output", Arg.Unit (add_to_map "truncate_output" "true"), "Truncate debug output.";
  "--no-truncate-output", Arg.Unit (add_to_map "truncate_output" "false"), "Do not truncate debug output.";
  "--test-email-addr", Arg.String (add_to_map_no_dummy "test_email_addr"), "Send a test email to the address and exit.";
] ;;

let main_server_args = [
  "--print-info",Arg.Unit (add_to_map "print_main_server_info" "true"),"Print debug info.";
  "--no-print-info", Arg.Unit (add_to_map "print_main_server_info" "false"), "Do not print debug info.";
  "--print-net-msgs", Arg.Unit (add_to_map "print_net_msgs" "true"), "Print messages for network events.";
  "--no-print-net-msgs", Arg.Unit (add_to_map "print_net_msgs" "false"), "Do not print messages for network events.";
  "--truncate-output", Arg.Unit (add_to_map "truncate_output" "true"), "Truncate debug output.";
  "--no-truncate-output", Arg.Unit (add_to_map "truncate_output" "false"), "Do not truncate debug output.";
  "--test-email-addr", Arg.String (add_to_map_no_dummy "test_email_addr"), "Send a test email to the address and exit.";
] ;;

let dp_server_args = [
  "--print-info", Arg.Unit (add_to_map "print_dp_server_info" "true"), "Print debug info.";
  "--no-print-info", Arg.Unit (add_to_map "print_dp_server_info" "false"), "Do not print debug info.";
  "--print-net-msgs", Arg.Unit (add_to_map "print_net_msgs" "true"), "Print messages for network events.";
  "--no-print-net-msgs", Arg.Unit (add_to_map "print_net_msgs" "false"), "Do not print messages for network events.";
  "--truncate-output", Arg.Unit (add_to_map "truncate_output" "true"), "Truncate debug output.";
  "--no-truncate-output", Arg.Unit (add_to_map "truncate_output" "false"), "Do not truncate debug output.";
] ;;

let parser_args = [ ];;

(* Sets the type of server we're running. *)
let set_server_type s_type = match s_type with
  | MainServer -> server_type := Some MainServer
  | DPServer -> server_type := Some DPServer
  | BothServers -> server_type := Some BothServers
  | Parser -> server_type := Some Parser ;;

(* Parses the command-line arguments. *)
let parse_cmd_line s_type =
  let usage_msg = "PiVC server command-line options:" in
  let fail_on_anon_arg arg =
    let error_msg = "Unknown anonymous option " ^ arg in
    raise (Arg.Bad error_msg)
  in
  let arg_list = match s_type with
    | MainServer -> main_server_args
    | DPServer -> dp_server_args
    | BothServers -> both_servers_args
    | Parser -> parser_args
  in
  Arg.parse arg_list fail_on_anon_arg usage_msg ;;

(* Sets up the config options by loading in the defaults
   from the specified file and then loading in arguments
   passed to the command line. *)
let load file_path = 
  key_value_map := read_from_file file_path;
  let add_cmd_line_vals key value =
    key_value_map := String_map.add key value !key_value_map
  in
  String_map.iter add_cmd_line_vals !cmd_line_arg_map ;;

(* Truncates the message if necessary. *)
let truncated_msg msg =
  let should_truncate = get_value_bool "truncate_output" in
  let truncate_finish = " [...]" in
  let truncate_len = (get_value_int "truncate_output_length") - (String.length truncate_finish) in
  if (should_truncate && (String.length msg) > truncate_len) then
    (Str.string_before (msg) truncate_len) ^ truncate_finish
  else
    msg ;;

let print msg =
  assert (Utils.is_some !server_type);
  (* Gets the string that we will print on the screen.
     Truncates it if necessary. *)
    (* Prints the final msg created above if the config map
       for key has value "true". *)
  let final_msg = truncated_msg msg in
  let print_if_true key =
    let print_fn = 
      if (String.length final_msg > 0 && final_msg.[String.length final_msg - 1] = '\n') then
	print_string
      else
	print_endline
    in
    let value = get_value_bool key in
    if value then print_fn final_msg
  in
  (* Get the right key based on what is running. *)
  let s_type = Utils.elem_from_opt !server_type in
  match s_type with
    | MainServer -> print_if_true "print_main_server_info"
    | DPServer -> print_if_true "print_dp_server_info"
    | BothServers -> print_endline msg
    | Parser -> print_endline msg ;;

let get_cmd_line_value key = 
  try
    String_map.find key (!cmd_line_arg_map)
  with
      Not_found -> raise (Cmd_Line_Arg_Key_Not_Found key)
        
let get_cmd_line_value_iff_key_exists key = 
  try
    Some(String_map.find key (!cmd_line_arg_map))
  with
      Not_found -> None

let always_print msg =
  print_endline (truncated_msg msg) ;;
  

