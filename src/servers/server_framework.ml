open Xml_generator
open Utils
open Ast
open Semantic_checking

let max_connections = 5000 ;;

(* Child/worker thread.  Handles one request. *)
let compile_thread (sock, server_fun) =
  let inchan = Unix.in_channel_of_descr sock
  and outchan = Unix.out_channel_of_descr sock in
  server_fun inchan outchan ;
  (*close_in inchan;
  close_out outchan;*)
  Unix.close sock ;;
    
let establish_server (server_fun: in_channel -> out_channel -> unit) sockaddr =
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  Unix.bind sock sockaddr;
  Unix.listen sock max_connections;
  try
    while true do
      let (s, _) = Unix.accept sock in
      if Config.get_value_bool "print_net_msgs" then
	Config.always_print "Accepted network request." ;
      ignore (Thread.create compile_thread (s, server_fun))
    done
  with
    Sys.Break -> (* Clean up on exit. *)
      Unix.close sock

let start_server serv_fun port =
  let my_address = Unix.inet_addr_any in
  print_endline ("Starting server on " ^ (Unix.string_of_inet_addr my_address) ^ ":" ^ (string_of_int port));
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
