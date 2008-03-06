open Xml_generator
open Utils
open Ast
open Semantic_checking

let default_port = 4242
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

let start_server serv_fun =
  let port = default_port in 
  let my_address = Unix.inet_addr_of_string "128.12.102.200" in
  print_endline ("Starting server on " ^ (Unix.string_of_inet_addr my_address) ^ ":" ^ (string_of_int port));
  establish_server serv_fun (Unix.ADDR_INET(my_address, port)) ;;

(* Converts from Windows to UNIX line endings. *)
let convert_line_endings str =
  Str.global_replace (Str.regexp "\r\n") "\n" str ;;

(* Gets one string of input from the stream.
   We first get an int (the number of chars)
   and then a string of that many chars. *)
let get_input ic = 
  let in_len = input_binary_int ic in
  print_endline ("Got input of length " ^ (string_of_int in_len));
  let in_buf = Buffer.create in_len in
  Buffer.add_channel in_buf ic in_len;
  convert_line_endings (Buffer.contents in_buf) ;;

(* Sends one string to the stream. *)
let send_output oc str =
  let out_len = String.length str in
  output_binary_int oc out_len;
  output_string oc str

(* Runs a server using the specified callback function
   to act as the server. *)
let run_server server_fun =
  Unix.handle_unix_error start_server server_fun
