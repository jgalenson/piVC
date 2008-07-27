type server_type =
  | MainServer
  | DPServer
  | BothServers
  | Parser ;;

(* Parse the command-line arguments.
   This should be called first. *)
val parse_cmd_line : server_type -> unit ;;

(* Sets the type of the currently-running server.
   This should be called second. *)
val set_server_type : server_type -> unit ;;

(* This should be called once, with the absolute path to the config file.
   This should be called third. *)
val load : string -> unit

(* Takes a config key, returns the corresponding value.
   Raises Config_Key_Not_Found if necessary. *)
val get_value : string -> string

(* Takes a config key, returns the corresponding value parsed to an integer.
   Raises Config_Key_Not_Found or Config_Key_Could_Not_Be_Parsed_To_Int if necessary. *)
val get_value_int : string -> int ;;

(* Takes a config key, returns the corresponding value parsed to a bool.
   Raises Config_Key_Not_Found or Config_Key_Could_Not_Be_Parsed_To_Bool if necessary. *)
val get_value_bool : string -> bool ;;

(* Prints the specified message if the config options to print are set.
   There are different options for the main and the dp server. *)
val print : string -> unit ;;

val get_cmd_line_value_iff_key_exists : string -> string option
  
val get_cmd_line_value : string -> string

(* Always prints the specified message.  Truncates if necessary. *)
val always_print : string -> unit ;;

val get_server_type : unit -> server_type option ;;

val is_main_server : unit -> bool ;;
val is_dp_server : unit -> bool ;;
