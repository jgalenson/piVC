type options = {
  generate_runtime_assertions : bool;
  find_inductive_core : bool;
}


val is_some : 'a option -> bool ;;
val is_none : 'a option -> bool ;;
val elem_from_opt : 'a option -> 'a ;;
val queue_to_list : 'a Queue.t -> 'a list ;;
val get_absolute_path : string -> string;;
val convert_line_endings : string -> string;;
val compare_locs : Lexing.position -> Lexing.position -> int;;
val trim : string -> string;;
val rational_string_of_float : float -> string;;
val debug_print_time_diff : float -> float -> string -> unit;;
