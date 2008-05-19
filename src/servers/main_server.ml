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
        (* Config.print_endline code; *)
      let (program, errors) = Compile.parse_strings [("user-file", code)] in
        
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
        Config.print "Compilation completed. Response sent back to client.";
    with
        ex -> 
          begin
            send_output oc (string_of_xml_node (xml_of_compiler_exception ex));
            Config.print ("Caught compiler exception: " ^ (Exceptions.string_of_exception ex))
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
    Xml_generator.set_text (Exceptions.string_of_exception ex) message_node;
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



and xml_of_verified_program (fns) = 
  (*Now we have the xml generation functions for the various levels*)
  let rec xml_of_function (fn) = 
    let function_node = Xml_generator.create "function" in
      add_attribute ("name", fn.fn.fnName.name) function_node;
      add_attribute ("status", Verify.string_of_validity fn.overall_validity) function_node;
      add_child (xml_of_location fn.fn.location_fd) function_node;
      let correctness_node = Xml_generator.create "correctness" in
      add_attribute ("status", Verify.string_of_validity fn.correctness_result.overall_validity_c) correctness_node;
      add_child correctness_node function_node;
      let process_vc vc = 
        add_child (xml_of_basic_path vc) correctness_node
      in
        List.iter process_vc fn.correctness_result.vcs;
	if (Utils.is_some fn.termination_result) then
	  add_child (xml_of_termination_info (Utils.elem_from_opt fn.termination_result)) function_node;
        function_node
  and xml_of_basic_path (vc) =
    let nodes = Basic_paths.get_steps_from_path (elem_from_opt vc.bp) in
    let basic_path_node = Xml_generator.create "basic_path" in
      add_attribute ("status", Verify.string_of_validity vc.valid) basic_path_node;
      let path_node = Xml_generator.create "path" in
        add_child path_node basic_path_node;
        let vc_node = xml_of_vc vc.vc in
          add_child vc_node basic_path_node;
	  if (Utils.is_some vc.counter_example) then
	    begin
	      add_child (xml_of_counterexample (Utils.elem_from_opt vc.counter_example)) basic_path_node
	    end;
          let process_step step = 
            add_child (xml_of_step step) path_node in
            List.iter process_step nodes;
            basic_path_node

  and xml_of_nonnegative_vc (vc) =
    let nonnegative_vc_node = Xml_generator.create "nonnegative_vc" in
      add_attribute ("status", Verify.string_of_validity vc.valid) nonnegative_vc_node;
      let vc_node = xml_of_vc vc.vc in
        add_child vc_node nonnegative_vc_node;
        begin
	  if (Utils.is_some vc.counter_example) then
	    add_child (xml_of_counterexample (Utils.elem_from_opt vc.counter_example)) nonnegative_vc_node
	end;
        let location_node = xml_of_location (location_of_vc_conjunct_list_list vc.vc) in
          add_child location_node nonnegative_vc_node;
          nonnegative_vc_node

  and xml_of_vc vc_conjunct_list_list =
    let vc_node = Xml_generator.create "vc" in
    let (lhs_list, rhs) = 
      let rev_list = List.rev vc_conjunct_list_list in
        (List.rev (List.tl rev_list), List.hd rev_list)
    in
      
    let xml_of_lhs_implies conjunct_list = 
      let implies_node = Xml_generator.create "implies" in
      let xml_of_lhs_conjunct conj = 
        let conjunct_node = Xml_generator.create "conjunct" in
        let text_node = Xml_generator.create "text" in
          set_text (string_of_expr conj.exp) text_node;
          add_child text_node conjunct_node;
          add_attribute ("in_inductive_core", string_of_bool !(conj.in_inductive_core)) conjunct_node;
          add_child (xml_of_location (location_of_expr conj.exp)) conjunct_node;
          conjunct_node
      in
      let add_conjunct_child conj = 
        add_child (xml_of_lhs_conjunct conj) implies_node
      in
        List.iter add_conjunct_child conjunct_list;
        implies_node
    in
    let xml_of_rhs_implies conjunct_list =
      let implies_node = Xml_generator.create "implies" in
      let xml_of_rhs_conjunct conj = 
        let conjunct_node = Xml_generator.create "rhs_conjunct" in
        let text_node = Xml_generator.create "text" in
          set_text (string_of_expr conj.exp) text_node;
          add_child text_node conjunct_node;
          add_attribute ("in_inductive_core", string_of_bool !(conj.in_inductive_core)) conjunct_node;
          add_attribute ("status", string_of_validity (elem_from_opt conj.valid_conjunct)) conjunct_node;
          add_child (xml_of_location (location_of_expr conj.exp)) conjunct_node;
          conjunct_node
      in
      let add_conjunct_child conj = 
        add_child (xml_of_rhs_conjunct conj) implies_node
      in
        List.iter add_conjunct_child conjunct_list;
        implies_node
    in
    let add_implies imp = 
      add_child (xml_of_lhs_implies imp) vc_node
    in
      List.iter add_implies lhs_list;
      add_child (xml_of_rhs_implies rhs) vc_node;
      vc_node
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
      add_attribute ("status", Verify.string_of_validity termination_info.overall_validity_t) termination_node;
      begin (* <decreasing> node *)
        let decreasing_node = Xml_generator.create "decreasing" in
	  add_attribute ("status", Verify.string_of_validity termination_info.decreasing_paths_validity) decreasing_node;
	  let process_vc vc = 
            add_child (xml_of_basic_path vc) decreasing_node
	  in
            List.iter process_vc (termination_info.decreasing_paths);    
            add_child decreasing_node termination_node;
      end;
      begin (* <nonnegative> node *)
        let nonnegative_node = Xml_generator.create "nonnegative" in
	  add_attribute ("status", Verify.string_of_validity (termination_info.nonnegative_vcs_validity)) nonnegative_node;
	  let process_vc vc =
            (*TODO-J: clean this up - can we just use xml_of_basic_path? *)
            (*let vc = vc_detailed.vc in TODO-J: uncomment when you're actually using the vc*)
            (*let valid = vc_detailed.valid in
              let counterexample = vc_detailed.counter_example in
	      let nonnegative_vc_node = Xml_generator.create "nonnegative_vc" in
	      add_child (xml_of_location (gdl())) nonnegative_vc_node; (*TODO-J: remove gdl()*)
	      let vc_node = Xml_generator.create "vc" in
              set_text ("vc goes here") vc_node;(*TODO-J: fix*)
              add_child vc_node nonnegative_vc_node;
	      add_attribute ("status", Verify.string_of_validity valid) nonnegative_vc_node;
	      if (Utils.is_some counterexample) then
	      begin
	      add_child (xml_of_counterexample (Utils.elem_from_opt counterexample)) nonnegative_vc_node
	      end;
              add_child nonnegative_vc_node nonnegative_node;*)
            add_child (xml_of_nonnegative_vc vc) nonnegative_node
	  in
	    List.iter process_vc (termination_info.nonnegative_vcs); 
	    add_child nonnegative_node termination_node;
      end;
      termination_node
  (* Now we put together the root node *)
  and transmission_node = Xml_generator.create "piVC_transmission" in
    add_attribute ("type", "program_submission_response") transmission_node;
    let result_node = Xml_generator.create "result" in
      add_attribute ("status", Verify.string_of_validity (Verify.overall_validity_of_function_validity_information_list fns)) result_node;
      add_child result_node transmission_node;
      let process_function func = 
        add_child (xml_of_function func) result_node
      in
        List.iter process_function fns;
        transmission_node ;;

(* Wrapper for compile function that passes it the
   cache of VCs. *)
let get_main_server_func () =
  let vc_cache = Hashtbl.create (Config.get_value_int "cache_size") in
  let cache_lock = Mutex.create () in
  compile (vc_cache, cache_lock) ;;

let start_main_server () =
  Config.load (Utils.get_absolute_path Constants.main_server_config_file_path) Config.MainServer;          
  run_server (get_main_server_func ()) (Config.get_value_int "port") ;;
