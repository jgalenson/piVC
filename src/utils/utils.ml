exception Option_Is_None ;;

(* Converts from Windows to UNIX line endings. *)
let convert_line_endings str =
  let str_temp = Str.global_replace (Str.regexp "\r\n") "\n" str in
    Str.global_replace (Str.regexp "\r") "\n" str

let get_absolute_path path = 
  let first_char = String.get path 0 in
    if first_char = '\\' then (*in this case, the path is already absolute*)
        path
    else (*otherwise we need to turn it into an absolute path*)
      (* We could use Sys.argv.(0) here. *)
      let path_of_executable = Sys.executable_name in
        (String.sub path_of_executable 0 ((String.rindex path_of_executable '/')+1)) ^ path 

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
