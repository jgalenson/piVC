open Xml_generator
open Utils
open Ast
open Semantic_checking

let max_connections = 10

(* The network code is stolen from http://caml.inria.fr/pub/docs/oreilly-book/html/book-ora187.html *)
    
let establish_server (server_fun: in_channel -> out_channel -> unit) sockaddr =
   let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
   Unix.bind sock sockaddr;
   Unix.listen sock max_connections;
   while true do
     let (s, caller) = Unix.accept sock in
     print_endline "Accepted" ; 
     match Unix.fork() with
       0 -> if Unix.fork() <> 0 then exit 0 ;
         let inchan = Unix.in_channel_of_descr s 
         and outchan = Unix.out_channel_of_descr s in
         server_fun inchan outchan ;
         (*close_in inchan;
         close_out outchan;*)
	 Unix.close s;
         exit 0
     | id -> Unix.close s; ignore(Unix.waitpid [] id)
   done

let get_my_addr () =
  (Unix.gethostbyname(Unix.gethostname())).Unix.h_addr_list.(0) ;;

let start_server serv_fun port =
  let my_address = get_my_addr () in
  print_endline ("Starting server on " ^ (Unix.string_of_inet_addr my_address) ^ ":" ^ (string_of_int port));
  establish_server serv_fun (Unix.ADDR_INET(my_address, port)) ;;

(* Runs a server using the specified callback function
   to act as the server. *)
let run_server server_fun port =
  Unix.handle_unix_error start_server server_fun port
