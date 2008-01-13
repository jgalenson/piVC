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
  establish_server serv_fun (Unix.ADDR_INET(my_address, port))

let read ic =
  let rec read_in () =
      let data = input_line ic in
      if String.get data 0 = Char.chr 4 then
	[]
      else
	data :: (read_in ())
  in
  read_in ()

let get_code ic =
  let code = read ic in
  String.concat "\n" code

let echo_service ic oc =
  let input = get_code ic in
  output_string oc input ; flush oc

(*let compile_service ic oc =
  Test_parser.goParse ic*)
    
let _ = Unix.handle_unix_error main_server echo_service
