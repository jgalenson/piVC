val is_some : 'a option -> bool ;;
val is_none : 'a option -> bool ;;
val elem_from_opt : 'a option -> 'a ;;
val queue_to_list : 'a Queue.t -> 'a list ;;
val truncate_for_printing : string -> string ;;
val get_absolute_path : string -> string;;