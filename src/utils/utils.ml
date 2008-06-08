open Lexing
exception Option_Is_None ;;

type options = {
  generate_runtime_assertions : bool;
  find_inductive_core : bool;
}


(* Converts from Windows to UNIX line endings. *)
let convert_line_endings str =
  let str_temp = Str.global_replace (Str.regexp "\r\n") "\n" str in
    Str.global_replace (Str.regexp "\r") "\n" str_temp

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
    if first_char = '/' then (*in this case, the path is already absolute*)
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

let rec trim str = 
  if str = "" || str = " " then str
  else
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
        trim (String.sub str start_index (end_index-start_index)) ;;

(* Turns a float into a string of its rational form
   e.g. 3.14 -> 314/100.  We don't do any reducing. *)
let rational_string_of_float f =
  let f_str = string_of_float f in
  let parts = Str.split_delim (Str.regexp_string ".") f_str in
  assert (List.length parts == 2);
  let prefix = List.nth parts 0 in
  let suffix = List.nth parts 1 in
  let numerator = prefix ^ suffix in
  let denominator = "1" ^ (String.make (String.length suffix) '0') in
  numerator ^ "/" ^ denominator ;;


let debug_print_time_diff begin_time end_time message =
  print_endline (message ^ (string_of_float (end_time -. begin_time)))

