open Xml_generator
open Utils



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
									
let rec compile ic oc =

  (* Convert queue of errors into a string. *)
  let get_error_string errors =
    let buf = Buffer.create 1024 in
    List.iter (fun e -> Buffer.add_string buf (Semantic_checking.string_of_error e)) errors;
    Buffer.contents buf
  in

  let code = get_input ic in
  (* print_endline code; *)
  let (program, errors) = Parse_utils.parse_string code in


  let get_output_to_return_to_client = 
    match errors with
        [] -> (
          let program_info = Parse_utils.get_all_info (Utils.elem_from_opt program) in
          let verified_program_info = Parse_utils.verify_program program_info in
            Xml_generator.string_of_xml_node (xml_of_verified_program verified_program_info)
              )
      | _  -> get_error_string errors

  in
    send_output oc get_output_to_return_to_client;
  flush oc


and xml_of_verified_program (all_valid, functions) = 
  (* We begin with a few utility functions *)
  let rec proved_of_bool bool = match bool with
      true -> "valid"
    | false -> "invalid"
     
  (*Now we have the xml generation functions for the various levels*)
  and xml_of_function (name, all_valid, basic_paths) = 
    let function_node = Xml_generator.create "function" in
      add_attribute ("name", name) function_node;
      let process_basic_path basic_path = 
        add_child (xml_of_basic_path basic_path) function_node in
        List.iter process_basic_path basic_paths;
        function_node
  and xml_of_basic_path (nodes, vc, valid) =
    let basic_path_node = Xml_generator.create "basic_path" in
      add_attribute ("status", proved_of_bool all_valid) basic_path_node;
      let path_node = Xml_generator.create "path" in
        add_child path_node basic_path_node;
        let vc_node = Xml_generator.create "vc" in
          set_text (Ast.string_of_expr vc) vc_node;
          add_child vc_node basic_path_node;
          let process_step step = 
            add_child (xml_of_step step) path_node in
            List.iter process_step nodes;
            basic_path_node
  and xml_of_step step = 
    let step_node = Xml_generator.create "step" in
      add_attribute ("type", Basic_paths.type_of_step step) step_node;
      add_child (xml_of_location (Basic_paths.location_of_path_node step)) step_node;
      let text_node = Xml_generator.create "text" in
        set_text (Basic_paths.string_of_path_node step) text_node;
        add_child text_node step_node;
        step_node
  and xml_of_location location = 
    let location_node = Xml_generator.create "location" in
    let start_node = Xml_generator.create "start" in
      add_attribute ("row", string_of_int location.Ast.loc_start.Lexing.pos_lnum) start_node;
      add_attribute ("col", string_of_int (Ast.col_number_of_position location.Ast.loc_start)) start_node;
      let end_node = Xml_generator.create "end" in
        add_attribute ("row", string_of_int location.Ast.loc_end.Lexing.pos_lnum) end_node;
        add_attribute ("col", string_of_int (Ast.col_number_of_position location.Ast.loc_end)) end_node;
        add_child start_node location_node;
        add_child end_node location_node;
        location_node
  (* Now we put together the root node *)
  and transmission_node = Xml_generator.create "piVC_transmission" in
    add_attribute ("type", "program_submission_response") transmission_node;
    let result_node = Xml_generator.create "result" in
      add_attribute ("status", proved_of_bool all_valid) result_node;
      add_child result_node transmission_node;
      let process_function func = 
        add_child (xml_of_function func) result_node in
        List.iter process_function functions;
        transmission_node

    
let _ = Unix.handle_unix_error main_server compile
