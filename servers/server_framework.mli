val run_server : (in_channel -> out_channel -> unit) -> unit ;;
val get_input : in_channel -> string ;;
val send_output : out_channel -> string -> unit
