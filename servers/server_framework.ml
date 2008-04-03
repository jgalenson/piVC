open Xml_generator
open Utils
open Ast
open Semantic_checking

let max_connections = 10

(* Taken from http://pleac.sourceforge.net/pleac_ocaml/sockets.html *)

(* Close all children. *)
let rec reaper signal =
  try while true do ignore (Unix.waitpid [Unix.WNOHANG] (-1)) done
  with Unix.Unix_error (Unix.ECHILD, _, _) -> ();
  Sys.set_signal Sys.sigchld (Sys.Signal_handle reaper)

(* The network code is stolen from http://caml.inria.fr/pub/docs/oreilly-book/html/book-ora187.html *)
    
let establish_server (server_fun: in_channel -> out_channel -> unit) sockaddr =
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  Unix.bind sock sockaddr;
  Unix.listen sock max_connections;
  try
    while true do
      let (s, caller) = Unix.accept sock in
      print_endline "Accepted" ; 
      match Unix.fork() with
	0 ->
	  Unix.close sock;
          let inchan = Unix.in_channel_of_descr s 
          and outchan = Unix.out_channel_of_descr s in
          server_fun inchan outchan ;
          (*close_in inchan;
           close_out outchan;*)
	   Unix.close s;
           exit 0
	| id -> Unix.close s; ignore(Unix.waitpid [] id)
    done
  with
    Sys.Break -> (* Clean up on exit. *)
      Unix.close sock;
      reaper Sys.sigint

let get_my_addr () =
  (Unix.gethostbyname(Unix.gethostname())).Unix.h_addr_list.(0) ;;

let start_server serv_fun port =
  let my_address = get_my_addr () in
  print_endline ("Starting server on " ^ (Unix.string_of_inet_addr my_address) ^ ":" ^ (string_of_int port));
  establish_server serv_fun (Unix.ADDR_INET(my_address, port)) ;;

let killed signal =
  print_endline "Killed";;

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
