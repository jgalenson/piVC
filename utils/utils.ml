exception Option_Is_None ;;


let get_absolute_path path_relative_to_dir_containing_executable = 
  let path_of_executable = Sys.argv.(0) in
    (String.sub path_of_executable 0 ((String.rindex path_of_executable '/')+1))   ^ path_relative_to_dir_containing_executable ;;

let is_some opt =
  match opt with
    | Some (_) -> true
    | None -> false ;;

let is_none opt =
  match opt with
    | Some (_) -> false 
    | None -> true ;;

let elem_from_opt opt = match opt with
    Some(something) -> something
  | None -> raise Option_Is_None ;;

(* Converts a queue to a list. *)
let queue_to_list q =
  Queue.fold (fun a b -> a @ [b]) [] q  ;;

let truncate_for_printing str = 
  if (String.length str) <= Constants.truncate_output_length then
    str
  else
    (Str.string_before str Constants.truncate_output_length) ^ ("... [tuncated]")
