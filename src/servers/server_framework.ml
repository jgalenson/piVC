open Xml_generator
open Utils
open Ast
open Semantic_checking

let max_connections = 10000 ;;

let string_of_inet_addr_port addr port =
  (Unix.string_of_inet_addr addr) ^ ":" ^ (string_of_int port) ;;

let string_of_sockaddr s = match s with
  | Unix.ADDR_UNIX (s) -> s
  | Unix.ADDR_INET (addr, port) -> string_of_inet_addr_port addr port ;;

let get_server_type_str () =
  let server_type = Config.get_server_type () in
  match server_type with
    | Some (Config.MainServer) -> "main "
    | Some (Config.DPServer) -> "dp "
    | _ -> "" ;;

(* Child/worker thread.  Handles one request. *)
let compile_thread (sock, server_fun, cli_addr) =

  let go_message msg = 
    if Config.get_value_bool "print_net_msgs" then
      Config.always_print msg;
    if Config.is_main_server () then
      Logger.log_info msg;
  in
    go_message ("Accepted network request from " ^ (string_of_sockaddr cli_addr) ^ ".");
    let inchan = Unix.in_channel_of_descr sock
    and outchan = Unix.out_channel_of_descr sock in
      server_fun inchan outchan ;
      (*close_in inchan;
        close_out outchan;*)
      Unix.close sock;
      go_message ("Finished processing network request from " ^ (string_of_sockaddr cli_addr) ^ ".")
        
let establish_server (server_fun: in_channel -> out_channel -> unit) sockaddr =
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  Unix.bind sock sockaddr;
  Unix.listen sock max_connections;
  try
    while true do
      let (s, cli_addr) = Unix.accept sock in
        ignore (Thread.create compile_thread (s, server_fun, cli_addr));
    done
  with
    Sys.Break -> (* Clean up on exit. *)
      Logger.log_info ("Closing " ^ get_server_type_str () ^ "server on " ^ (string_of_sockaddr sockaddr) ^ ".");
      Unix.close sock

let start_server serv_fun port =
  let my_address = Unix.inet_addr_any in
  let msg = "Starting " ^ get_server_type_str () ^ "server on " ^ (string_of_inet_addr_port my_address port) ^ "." in
  print_endline msg;
  Logger.log_info msg;
  establish_server serv_fun (Unix.ADDR_INET(my_address, port)) ;;

(* Runs a server using the specified callback function
   to act as the server. *)
let run_server server_fun port =
  try
    Sys.catch_break true;
    start_server server_fun port
  with
    | Unix.Unix_error (e, fn, params) -> (* Print error messages. *)
	print_endline ("Unix error: " ^ (Unix.error_message e));
	raise (Unix.Unix_error (e, fn, params))
