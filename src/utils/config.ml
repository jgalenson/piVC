module String_map = Map.Make(String)

exception Config_Key_Not_Found of string;;
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

let rec trim str = 
  let start_index = match String.get str 0 with
      ' ' -> 1
    | _ -> 0
  in
  let end_index = match String.get str ((String.length str) - 1) with
      ' ' -> (String.length str) - 1
    | _ -> String.length str
  in
    if start_index = 0 && end_index = String.length str then
      str
    else
      trim (String.sub str start_index (end_index-start_index))
  
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
      let key_value = String.sub str 0 index_of_end in
      let rest = 
        if index_of_end < (String.length str)-1 then
          String.sub str (index_of_end + 1) ((String.length str)-index_of_end-1)
        else
          ""
      in
        if (String.length key_value > 0) && (String.get key_value 0 != '#') then
          begin
            let index_of_equals = String.index key_value '=' in
            let key = trim (String.sub key_value 0 index_of_equals) in 
            let value = trim (String.sub key_value (index_of_equals + 1) ((String.length key_value)-index_of_equals-1)) in
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
  | Parser ;;

let server_type = ref None ;;

let is_main_server () =
  server_type := Some MainServer ;;

let is_dp_server () =
  server_type := Some DPServer ;;

(* Code to parse command-line arguments. *)

let add_to_map key value () =
  key_value_map := String_map.add key value !key_value_map ;;

(* The command-line arguments we accept. *)
let speclist = [
  "--print-main-server-info",Arg.Unit (add_to_map "print_main_server_info" "true"),"Print debug info in main server.";
  "--no-print-main-server-info", Arg.Unit (add_to_map "print_main_server_info" "false"), "Do not print debug info in main server.";
  "--print-dp-server-info", Arg.Unit (add_to_map "print_dp_server_info" "true"), "Print debug info in dp server.";
  "--no-print-dp-server-info", Arg.Unit (add_to_map "print_dp_server_info" "false"), "Do not print debug info in dp server.";
  "--truncate-output", Arg.Unit (add_to_map "truncate_output" "true"), "Truncate debug output.";
  "--no-truncate-output", Arg.Unit (add_to_map "truncate_output" "false"), "Do not truncate debug output.";
]

let parse_cmd_line () =
  let usage_msg = "PiVC server command-line options:" in
  let fail_on_anon_arg arg =
    let error_msg = "Unknown anonymous option " ^ arg in
    raise (Arg.Bad error_msg)
  in
    Arg.parse speclist fail_on_anon_arg usage_msg ;;

(* Sets up the config options by loading in the defaults
   from the specified file and then loading in arguments
   passed to the command line. *)
let load file_path s_type = 
  key_value_map := read_from_file file_path;
  begin
    match s_type with
      | MainServer -> server_type := Some MainServer
      | DPServer -> server_type := Some DPServer
      | Parser -> server_type := Some Parser
  end;
  parse_cmd_line () ;;

let print msg =
  assert (Utils.is_some !server_type);
  (* Gets the string that we will print on the screen.
     Truncates it if necessary. *)
  let final_msg =
    let should_truncate = get_value_bool "truncate_output" in
    let truncate_finish = " [...]" in
    let truncate_len = (get_value_int "truncate_output_length") - (String.length truncate_finish) in
      if (should_truncate && (String.length msg) > truncate_len) then
        (Str.string_before (msg) truncate_len) ^ truncate_finish
      else
        msg
  in
    (* Prints the final msg created above if the config map
       for key has value "true". *)
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
    | Parser -> print_endline msg ;;

