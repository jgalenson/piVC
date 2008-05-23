open Lexing
exception Option_Is_None ;;

type options = {
  generate_runtime_assertions : bool;
  find_inductive_core : bool;
}

(* Converts from Windows to UNIX line endings. *)
let convert_line_endings str =
  let str_temp = Str.global_replace (Str.regexp "\r\n") "\n" str in
    Str.global_replace (Str.regexp "\r") "\n" str

let compare_locs loc1 loc2 = 
  let fname_compr = String.compare (loc1.pos_fname) (loc2.pos_fname) in
  let lnum_compr = loc1.pos_lnum - loc2.pos_lnum in 
  let bol_compr = loc1.pos_bol - loc2.pos_bol in 
  let cnum_compr = loc1.pos_cnum - loc2.pos_cnum in 
    if fname_compr != 0 then
      fname_compr
    else if lnum_compr != 0 then
      lnum_compr
    else if bol_compr != 0 then
      bol_compr
    else
      cnum_compr

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
