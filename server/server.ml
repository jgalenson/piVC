let default_port = 4242

(* The network code is stolen from http://caml.inria.fr/pub/docs/oreilly-book/html/book-ora187.html *)
    
let establish_server server_fun sockaddr =
   let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
   Unix.bind sock sockaddr;
   Unix.listen sock 3;
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

let main_server serv_fun =
  let port = default_port in 
  let my_address = get_my_addr () in
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
  print_endline (string_of_int in_len);
  let in_buf = Buffer.create in_len in
  Buffer.add_channel in_buf ic in_len;
  convert_line_endings (Buffer.contents in_buf) ;;

(* Sends one string to the stream. *)
let send_output oc str =
  let out_len = String.length str in
  output_binary_int oc out_len;
  output_string oc str
									
let compile ic oc =

  (* Convert queue of errors into a string. *)
  let get_error_string errors =
    let buf = Buffer.create 1024 in
    Queue.iter (fun e -> Buffer.add_string buf (Semantic_checking.string_of_error e)) errors;
    Buffer.contents buf
  in

  let code = get_input ic in
  (* print_endline code; *)
  let (program, errors) = Parse_utils.parse_string code in
  let error_string = get_error_string errors in
  (* print_string error_string; *)
  send_output oc error_string;
  flush oc
    
let _ = Unix.handle_unix_error main_server compile
