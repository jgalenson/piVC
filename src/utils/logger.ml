open Unix ;;

let log_lock = Mutex.create () ;;

(* Gets a string out of the current time.
   It is of the form: Jun 4 16:21:42 *)
let get_time_str () =
  let time = Unix.localtime (Unix.time ()) in
  let month_str = match time.tm_mon with
    | 0 -> "Jan"
    | 1 -> "Feb"
    | 2 -> "Mar"
    | 3 -> "Apr"
    | 4 -> "May"
    | 5 -> "Jun"
    | 6 -> "Jul"
    | 7 -> "Aug"
    | 8 -> "Sep"
    | 9 -> "Oct"
    | 10 -> "Nov"
    | 11 -> "Dec"
    | _ -> assert false (* Month is only 0-11 as given in docs. *)
  in
  (* Pad length 1 numbers to length 2. *)
  let soi x =
    let str = string_of_int x in
    if String.length str = 1 then
      "0" ^ str
    else
      str
  in
  month_str ^ " " ^ (string_of_int time.tm_mday) ^ " " ^ (soi time.tm_hour) ^ ":" ^ (soi time.tm_min) ^ ":" ^ (soi time.tm_sec) ;;

(* Gets a string of the currently-running server. *)
let get_server_type_str () =
    let server_type = Config.get_server_type () in
    match server_type with
      | Some (Config.MainServer) -> "main server"
      | Some (Config.DPServer) -> "dp server"
      | _ -> "unknown server" ;;

(* Creates a tree of directories for the given filename.
   That is, if called with "a/b/c/d", we ensure that
   the directories "a/b/c" exist.
   Call this before Unix.openfile, since that fails
   if the directories do not exist. *)
let create_dir_tree filename =
  let rec create_dir prefix rest =
    begin
      try
        Unix.umask 0o0;
        Unix.mkdir prefix 0o700
      with _ -> ignore () (* If it already exists, we're fine with that. *)
    end;
    if List.length rest > 0 then
      let new_prefix =
	let divider = if String.length prefix > 0 then "/" else "" in
	prefix ^ divider ^ (List.hd rest)
      in
      create_dir new_prefix (List.tl rest)
  in
  let dir_names = Filename.dirname filename in
  let parts = Str.split (Str.regexp "/") dir_names in
  create_dir "" parts ;;

(* filename is already absolute. *)
let log msg filename =
  let is_enabled = Config.get_value_bool "logging" in
  if is_enabled then begin
    Mutex.lock log_lock;
    create_dir_tree filename;
    Unix.umask 0o0;
    let log_file = Unix.openfile filename [Unix.O_CREAT; Unix.O_WRONLY; Unix.O_APPEND] 0o640 in
    let write_to_log fd str =
      let full_str = get_time_str () ^ " " ^ get_server_type_str () ^ ": " ^ str ^ "\n" in
      ignore (Unix.write fd full_str 0 (String.length full_str))
    in
    write_to_log log_file msg;
    Unix.close log_file;
    Mutex.unlock log_lock
  end ;;

let log_info msg =
  let filename = Config.get_value "info_log_file" in
  log msg filename ;;

let log_error msg =
  let filename = Config.get_value "error_log_file" in
  log msg filename ;;
