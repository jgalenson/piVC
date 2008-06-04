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
  let soi = string_of_int in
  (* Pad length 1 numbers to length 2. *)
  let soi_pad x =
    let str = string_of_int x in
    if String.length str = 1 then
      "0" ^ str
    else
      str
  in
  month_str ^ " " ^ (string_of_int time.tm_mday) ^ " " ^ (soi_pad time.tm_hour) ^ ":" ^ (soi time.tm_min) ^ ":" ^ (soi_pad time.tm_sec) ;;

(* Gets a string of the currently-running server. *)
let get_server_type_str () =
    let server_type = Config.get_server_type () in
    match server_type with
      | Some (Config.MainServer) -> "main server"
      | Some (Config.DPServer) -> "dp server"
      | _ -> "unknown server" ;;

let log msg =
  print_endline "Hi";
  (* TODO: Check Config to see if logging is enabled. *)
  (* TODO: Get logging filename from Config. *)
  let filename = "pivc.log" in
  Mutex.lock log_lock;
  let log_file = Unix.openfile filename [Unix.O_CREAT; Unix.O_WRONLY; Unix.O_APPEND] 0o640 in
  let write_to_log fd str =
    let full_str = get_time_str () ^ " " ^ get_server_type_str () ^ ": " ^ str ^ "\n" in
    ignore (Unix.write fd full_str 0 (String.length full_str))
  in
  write_to_log log_file msg;
  Unix.close log_file;
  Mutex.unlock log_lock
