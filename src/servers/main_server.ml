open Xml_generator
open Utils
open Ast
open Semantic_checking
open Server_framework
open Net_utils
open Verify ;;

exception InvalidXml of string ;;

(*
 * Code for the main server.
 * Gets input from a client, builds bps and vcs,
 * sends them to the verifier, and returns the result
 * (or a compile error).
 *)

let rec compile vc_cache_and_lock ic oc =
  (* Convert queue of errors into a string. *)
  (*let get_error_string errors =
    let buf = Buffer.create 1024 in
    List.iter (fun e -> Buffer.add_string buf (Semantic_checking.string_of_error e)) errors;
    Buffer.contents buf
  in*)

  (* Parse the xml we get from the client. *)
  let parse_xml xml_str =
    (* Replace \rs. *)
    let replace_bad_chars str = 
      Str.global_replace (Str.regexp "&#13;") "" str
    in
    (* Returns whether or not xml has a child tag named tag. *)
    let has_child tag xml =
      try
	ignore (List.find (function n -> Xml.tag n = tag) (Xml.children xml));
	true
      with Not_found -> false
    in
    (* Ensure the xml we got from the client has a valid root tag. *)
    let check_xml xml =
      assert (Xml.tag xml = "piVC_transmission");
      assert (has_child "code" xml);
      assert (has_child "options" xml);      
      try
	assert (Xml.attrib xml "type" = "program_submission_request");
      with _ -> raise (InvalidXml "No type attribute in xml from client.");
    in
    (* Gets the child node named tag from the node xml. *)
    let get_child_node tag xml =
      try
	List.find (function n -> Xml.tag n = tag) (Xml.children xml)
      with _ -> raise (InvalidXml ("No " ^ tag ^ " tag in xml from client."))
    in
    (* Gets the text from a text node.
       We assume a text node has one child, which is its text. *)
    let get_text_from_text_node node =
      let text = Xml.pcdata (List.hd (Xml.children node)) in
      replace_bad_chars text
    in
    let xml = Xml.parse_string xml_str in
    check_xml xml;
    let code_node = get_child_node "code" xml in
    let code = get_text_from_text_node code_node in
    (* Gets the options. *)
    let options =
      let option_node = get_child_node "options" xml in
      let should_generate_runtime_assertions = has_child "generate_runtime_assertions" option_node in
	(should_generate_runtime_assertions)
    in
    (code, options)
  in
  
  begin
    try    
      let xml_str = get_input ic in
      let (code, (gen_runtime_asserts)) = parse_xml xml_str in
        (* print_endline code; *)
      let (program, errors) = Parse_utils.parse_strings [("user-file", code)] in
        
      let get_output_to_return_to_client = 
        match errors with
            [] -> (
              let program_info = Verify.get_all_info (Utils.elem_from_opt program) gen_runtime_asserts in
              let verified_program_info = Verify.verify_program program_info (Utils.elem_from_opt program) vc_cache_and_lock in
                Xml_generator.string_of_xml_node (xml_of_verified_program verified_program_info)
            )
          | _  -> Xml_generator.string_of_xml_node (xml_of_errors errors)
              
      in
        send_output oc get_output_to_return_to_client;
        print_endline "Compilation completed. Response sent back to client.";
    with
        ex -> 
          begin
            send_output oc (string_of_xml_node (xml_of_compiler_exception ex));
            print_endline ("Caught compiler exception: " ^ (Printexc.to_string ex))
          end
  end;
  flush stdout;
  flush stderr;
  flush oc
    


and xml_of_location location = 
  let location_node = Xml_generator.create "location" in
  let start_node = Xml_generator.create "start" in
    add_attribute ("row", string_of_int location.Ast.loc_start.Lexing.pos_lnum) start_node;
    add_attribute ("col", string_of_int (Ast.col_number_of_position location.Ast.loc_start)) start_node;
    add_attribute ("byte", string_of_int location.Ast.loc_start.Lexing.pos_cnum) start_node;
    let end_node = Xml_generator.create "end" in
      add_attribute ("row", string_of_int location.Ast.loc_end.Lexing.pos_lnum) end_node;
      add_attribute ("col", string_of_int (Ast.col_number_of_position location.Ast.loc_end)) end_node;
      add_attribute ("byte", string_of_int location.Ast.loc_end.Lexing.pos_cnum) end_node;
      add_child start_node location_node;
      add_child end_node location_node;
      location_node

and xml_of_compiler_exception ex = 
  let transmission_node = Xml_generator.create "piVC_transmission" in
    add_attribute ("type", "program_submission_response") transmission_node;
  let result_node = Xml_generator.create "result" in  
    add_attribute ("status", "compiler_error") result_node;
  let error_node = Xml_generator.create "error" in
    add_attribute ("type", "compiler_error") error_node;
  let message_node = Xml_generator.create "message" in
    Xml_generator.set_text (Printexc.to_string ex) message_node;
  add_child message_node error_node;
  add_child error_node result_node;
  add_child result_node transmission_node;
  transmission_node
      
and xml_of_errors errors =

  let transmission_node = Xml_generator.create "piVC_transmission" in
    add_attribute ("type", "program_submission_response") transmission_node;
    let result_node = Xml_generator.create "result" in
      add_attribute ("status", "error") result_node;
      List.iter (fun e -> 
                   let error_node = (Xml_generator.create "error") in
                     Xml_generator.add_attribute ("type", (Semantic_checking.string_of_error_type_for_xml e.e_type)) error_node;
                     Xml_generator.add_child (xml_of_location e.loc) error_node;
                     let message_node = Xml_generator.create "message" in
                       Xml_generator.set_text e.msg message_node;
                       Xml_generator.add_child message_node error_node;
                       Xml_generator.add_child error_node result_node) errors;                     
      Xml_generator.add_child result_node transmission_node;
      transmission_node



and xml_of_verified_program (all_valid, functions) = 
  (*Now we have the xml generation functions for the various levels*)
  let rec xml_of_function (fn, all_valid, (all_paths_valid, basic_paths), termination_info_opt) = 
    let function_node = Xml_generator.create "function" in
      add_attribute ("name", fn.fnName.name) function_node;
      add_attribute ("status", Verify.string_of_validity all_valid) function_node;
      add_child (xml_of_location fn.location_fd) function_node;
      let correctness_node = Xml_generator.create "correctness" in
      add_attribute ("status", Verify.string_of_validity all_paths_valid) correctness_node;
      add_child correctness_node function_node;
      let process_basic_path basic_path = 
        add_child (xml_of_basic_path basic_path) correctness_node
      in
        List.iter process_basic_path basic_paths;
	if (Utils.is_some termination_info_opt) then
	  add_child (xml_of_termination_info (Utils.elem_from_opt termination_info_opt)) function_node;
        function_node
  and xml_of_basic_path (path, vc, valid, counterexample) =
    let nodes = Basic_paths.get_steps_from_path path in
    let basic_path_node = Xml_generator.create "basic_path" in
      add_attribute ("status", Verify.string_of_validity valid) basic_path_node;
      let path_node = Xml_generator.create "path" in
        add_child path_node basic_path_node;
        let vc_node = Xml_generator.create "vc" in
          set_text (Ast.string_of_expr vc) vc_node;
          add_child vc_node basic_path_node;
	  if (Utils.is_some counterexample) then
	    begin
	      add_child (xml_of_counterexample (Utils.elem_from_opt counterexample)) basic_path_node
	    end;
          let process_step step = 
            add_child (xml_of_step step) path_node in
            List.iter process_step nodes;
            basic_path_node
  and xml_of_step step = 
    let step_node = Xml_generator.create "step" in
      add_attribute ("type", Basic_paths.type_of_step step) step_node;
      add_child (xml_of_location (Basic_paths.location_of_path_step step)) step_node;
      let text_node = Xml_generator.create "text" in
        set_text (Basic_paths.string_of_path_step step) text_node;
        add_child text_node step_node;
        step_node
  and xml_of_counterexample counterexample =
    let counterexample_node = Xml_generator.create "counterexample" in
    let process_var ex =
      let var_node = Xml_generator.create "var" in
      add_attribute ("text", Counterexamples.example_to_string ex) var_node;
      add_child (xml_of_location (Counterexamples.location_of_example ex)) var_node;
      add_child var_node counterexample_node;
      ()
    in
      List.iter process_var counterexample;
      counterexample_node
  (* <termination> node *)
  and xml_of_termination_info termination_info =
    let termination_node = Xml_generator.create "termination" in
    add_attribute ("status", Verify.string_of_validity termination_info.overall_validity) termination_node;
      begin (* <decreasing> node *)
      let decreasing_node = Xml_generator.create "decreasing" in
	add_attribute ("status", Verify.string_of_validity (fst termination_info.decreasing_paths)) decreasing_node;
	let process_basic_path basic_path = 
          add_child (xml_of_basic_path basic_path) decreasing_node
	in
        List.iter process_basic_path (snd termination_info.decreasing_paths);    
        add_child decreasing_node termination_node;
    end;
      begin (* <nonnegative> node *)
      let nonnegative_node = Xml_generator.create "nonnegative" in
	add_attribute ("status", Verify.string_of_validity (fst termination_info.nonnegative_vcs)) nonnegative_node;
	let process_vc (vc, valid, counterexample) =
	  let nonnegative_vc_node = Xml_generator.create "nonnegative_vc" in
	  add_child (xml_of_location (Ast.location_of_expr vc)) nonnegative_vc_node;
	  let vc_node = Xml_generator.create "vc" in
          set_text (Ast.string_of_expr vc) vc_node;
          add_child vc_node nonnegative_vc_node;
	  add_attribute ("status", Verify.string_of_validity valid) nonnegative_vc_node;
	  if (Utils.is_some counterexample) then
	    begin
	      add_child (xml_of_counterexample (Utils.elem_from_opt counterexample)) nonnegative_vc_node
	    end;
          add_child nonnegative_vc_node nonnegative_node;
	in
	  List.iter process_vc (snd termination_info.nonnegative_vcs); 
	add_child nonnegative_node termination_node;
    end;
    termination_node
  (* Now we put together the root node *)
  and transmission_node = Xml_generator.create "piVC_transmission" in
    add_attribute ("type", "program_submission_response") transmission_node;
    let result_node = Xml_generator.create "result" in
      add_attribute ("status", Verify.string_of_validity all_valid) result_node;
      add_child result_node transmission_node;
      let process_function func = 
        add_child (xml_of_function func) result_node
      in
        List.iter process_function functions;
        transmission_node ;;

(* Wrapper for compile function that passes it the
   cache of VCs. *)
let get_main_server_func () =
  let vc_cache = Hashtbl.create (Config.get_value_int "cache_size") in
  let cache_lock = Mutex.create () in
  compile (vc_cache, cache_lock)