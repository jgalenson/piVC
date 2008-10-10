open Xml_generator
open Utils
open Ast
open Semantic_checking
open Server_framework
open Net_utils
open Email
open Verify ;;

exception InvalidXml of string ;;
exception Main_Server_Timeout ;;

(*
 * Code for the main server.
 * Gets input from a client, builds bps and vcs,
 * sends them to the verifier, and returns the result
 * (or a compile error).
 *)

let rec compile vc_cache_and_lock ic oc =

  (* Parse the xml we get from the client. *)
  let parse_xml xml_str =
    let replace_bad_chars str = 
      (*client puts in spirious \b chars to circumvent bug in xml parser*)
      Str.global_replace (Str.regexp "&#8;") "" str
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
      try
        assert (Xml.tag xml = "piVC_transmission");
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
      match List.length (Xml.children node) with
          0 -> "" (*if the file is empty, the text won't register as a child, even though it should*)
        | _ ->
            begin
              let text = Xml.pcdata (List.hd (Xml.children node)) in
                replace_bad_chars text
            end
    in
    let xml = Xml.parse_string xml_str in
      check_xml xml;
      let code = 
        match has_child "code" xml with
            true -> let code_node = get_child_node "code" xml in
              Some(get_text_from_text_node code_node)
          | false -> None
              (* Gets the options. *)
      in
      let (options,submission_info) =
        match has_child "options" xml with
            true -> 
              begin
                let options_node = get_child_node "options" xml in
                let should_generate_runtime_assertions = has_child "generate_runtime_assertions" options_node in
                let should_find_inductive_core = has_child "find_inductive_core" options_node in
                let submission_info = 
                  match has_child "submit" options_node with
                      false -> None
                    | true ->
                        begin
                          let submit_node = get_child_node "submit" options_node in
                          let addrs_node = get_child_node "to_addrs" submit_node in
                          let to_addrs = ref [] in
                            List.iter
                              (function node -> if Xml.tag node = "addr" then to_addrs := !to_addrs @ [get_text_from_text_node node])
                              (Xml.children addrs_node);
                            let comment = match has_child "comment" submit_node with
                                false -> None
                              | true -> Some(get_text_from_text_node (get_child_node "comment" submit_node))
                            in
                              Some({to_addrs=to_addrs.contents; comment=comment})
                        end
                in
	          (Some({generate_runtime_assertions = should_generate_runtime_assertions; find_inductive_core = should_find_inductive_core;}), submission_info)
              end
          | false -> (None,None)
      in
      let user_info = 
        match has_child "user" xml with
            false -> None
          | true ->
              begin
                let user_node = get_child_node "user" xml in
                  Some({user_name = Xml.attrib user_node "name"; user_email_addr = Xml.attrib user_node "email_addr"})
              end
      in
      let report_info = 
        match has_child "report_type" xml with
            false -> None
          | true ->
              begin
                let report_type_node = get_child_node "report_type" xml in
                let report_type =
                  match get_text_from_text_node report_type_node with
                      "bug" -> Bug_report
                    | "feedback" -> Feedback
                    | _ -> assert false
                in
                let report_comment =
                  match has_child "comment" xml with
                      false -> None
                    | true ->
                        begin
                          let report_comment_node = get_child_node "comment" xml in
                            Some(get_text_from_text_node report_comment_node)
                        end
                in
                  Some({report_type = report_type; report_comment = report_comment})
              end
      in
        (code, options, user_info, submission_info, report_info)
  in    
  let go_exception xml_str ex =
    try
      let log_message = 
        "An exception has occured.\n\n" ^
          Email.email_heading "Exception" ^
          Exceptions.string_of_exception ex ^
          "\n\n" ^
          Email.email_heading "Request XML" ^ xml_str
      in
        send_output oc (string_of_xml_node (xml_of_compiler_exception ex));
        Config.print ("Caught compiler exception: " ^ (Exceptions.string_of_exception ex));
        Logger.log_error log_message;
        Email.send_error_notification log_message
    with
        ex_inner ->
          begin
            (*Note: I purposely use print_endline as opposed to Config.print. This code is supposed
              to catch exceptions in (among other things) Config.print, so we dont want to use
              the code that potentially caused an exception to respond to an exception.
              This code is for a super-worst case scenario. It will probably never be run in the
              entire (hopefully long) lifetime of piVC.*)
            print_endline "Critical issue: there was an exception while processing another exception.";
            print_endline ("The original exception was: " ^ (Exceptions.string_of_exception ex));
            print_endline ("The exception within the exception was: " ^ (Exceptions.string_of_exception ex_inner))
          end
  in
  begin
      try
	ignore (Sys.set_signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise Main_Server_Timeout)));
	ignore (Unix.alarm (Config.get_value_int "timeout_time"));
        let xml_str = get_input ic in
        try
          let xml_to_return =
            let (code, options, user_info, submission_info, report_info) = parse_xml xml_str in
            (*print_endline xml_str;
              print_endline (elem_from_opt code);*)
            match report_info with 
                Some(report_info) -> xml_of_messages_transmission [go_report report_info code user_info options]
              | None ->
                  begin
                    let messages = ref [] in
                    let options = Utils.elem_from_opt options in
                    let code = Utils.elem_from_opt code in
                    let (program, errors) = Compile.parse_strings [("user-file", code)] in
                    match errors with
                        [] -> 
                          begin
                            let program_info = Verify.get_all_info (Utils.elem_from_opt program) options in
                            let verified_program_info = Verify.verify_program program_info (Utils.elem_from_opt program) vc_cache_and_lock options in
                            begin
                              match submission_info with
                                  Some(s) -> 
                                    let msg = Email.go_submit code s (elem_from_opt user_info) options (Some(verified_program_info)) [] in
                                    messages := messages.contents @ [msg]
                                | None -> ignore()
                            end;
                            begin
                              if (Verify.overall_validity_of_function_validity_information_list verified_program_info != Valid) && options.find_inductive_core && Verify.inductive_core_good_enough verified_program_info then
                                messages := messages.contents @ [Constants.inductive_core_message]
                            end;
                            begin
                              if Verify.contains_unknown_vc verified_program_info then
                                messages := messages.contents @ [Constants.unknown_message]
                            end;
                            xml_of_verified_program verified_program_info messages.contents
                          end
                      | _  -> 
                          begin
                            begin
                              match submission_info with
                                  Some(s) -> 
                                    let msg = Email.go_submit code s (elem_from_opt user_info) options None errors in
                                    messages := messages.contents @ [msg]
                                | None -> ignore()
                            end;
                            xml_of_errors errors messages.contents
                          end
                  end
          in
            send_output oc (string_of_xml_node xml_to_return);
            Config.print "Compilation completed. Response sent back to client.";
        with ex -> go_exception xml_str ex;
      with ex ->  go_exception "No XML is available. The exception occured before the transmission had been fully recieved." ex
  end;
    Sys.set_signal Sys.sigalrm Sys.Signal_ignore;
    flush stdout;
    flush stderr;
    flush oc;
  
and xml_of_messages messages = 
  let messages_node = Xml_generator.create "messages" in
  let add_message message = 
    let message_node = Xml_generator.create "message" in
      set_text message message_node;
      add_child message_node messages_node
  in
    List.iter add_message messages;
    messages_node

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
      
and xml_of_errors errors messages =
  let transmission_node = Xml_generator.create "piVC_transmission" in
    add_attribute ("type", "program_submission_response") transmission_node;
    let messages_node = xml_of_messages messages in
      add_child messages_node transmission_node;
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
        
and xml_of_messages_transmission messages = 
  let transmission_node = Xml_generator.create "piVC_transmission" in
    add_attribute ("type", "messages") transmission_node;
    let messages_node = xml_of_messages messages in
      add_child messages_node transmission_node;
      transmission_node
        
and xml_of_verified_program fns messages =
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
        add_child (xml_of_verification_atom vc) correctness_node
      in
        List.iter process_vc fn.correctness_result.vcs;
	if (Utils.is_some fn.termination_result) then
	  add_child (xml_of_termination_info (Utils.elem_from_opt fn.termination_result)) function_node;
        function_node
  and xml_of_verification_atom (vc) =
    let atom_node = Xml_generator.create "verification_atom" in
      add_attribute ("name", Verify.name_of_verification_atom vc) atom_node;
      add_attribute ("status", Verify.string_of_validity vc.valid) atom_node;
      let vc_node = xml_of_vc vc.vc in
        add_child vc_node atom_node;
        let location_node = xml_of_location (location_of_verification_atom vc) in
          add_child location_node atom_node;
          begin
            if (Utils.is_some vc.counter_example) then
	      add_child (xml_of_counterexample (Utils.elem_from_opt vc.counter_example)) atom_node
	  end;
          begin
            match vc.info with
                BP(bp) ->
                  begin
                    let nodes = Basic_paths.get_steps_from_path bp in
                    let path_node = Xml_generator.create "basic_path" in
                      add_child path_node atom_node;
                      let process_step step = 
                        add_child (xml_of_step step) path_node
                      in
                        List.iter process_step nodes
                  end
              | RankingFunc(r) -> ignore()
          end;
          atom_node
  and xml_of_vc vc_conjunct_list_list =
    let vc_node = Xml_generator.create "vc" in      
    let xml_of_conjunct_list conjunct_list = 
      let implies_node = Xml_generator.create "implies" in
      let xml_of_conjunct conj = 
        let conjunct_node = Xml_generator.create "conjunct" in
        let text_node = Xml_generator.create "text" in
          set_text (string_of_expr conj.exp) text_node;
          add_child text_node conjunct_node;
          begin
            match conj.in_inductive_core with
                Some(c) -> add_attribute ("in_inductive_core", string_of_bool c.contents) conjunct_node
              | _ -> ignore()
          end;
          begin
            match conj.valid_conjunct with
                Some(v) -> add_attribute ("status", string_of_validity v) conjunct_node
              | _ -> ignore()
          end;
          add_child (xml_of_location (location_of_expr conj.exp)) conjunct_node;
          conjunct_node
      in
      let add_conjunct_child conj = 
        add_child (xml_of_conjunct conj) implies_node
      in
        List.iter add_conjunct_child conjunct_list;
        implies_node
    in
    let add_implies imp = 
      add_child (xml_of_conjunct_list imp) vc_node
    in
      List.iter add_implies vc_conjunct_list_list;
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
      add_attribute ("text", Smt_solver.Counterexample.example_to_string ex) var_node;
      let loc = Smt_solver.Counterexample.location_of_example ex in
      if Utils.is_some loc then
	add_child (xml_of_location (Utils.elem_from_opt loc)) var_node;
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
            add_child (xml_of_verification_atom vc) decreasing_node
	  in
            List.iter process_vc (termination_info.decreasing_paths);    
            add_child decreasing_node termination_node;
      end;
      begin (* <nonnegative> node *)
        let nonnegative_node = Xml_generator.create "nonnegative" in
	  add_attribute ("status", Verify.string_of_validity (termination_info.nonnegative_vcs_validity)) nonnegative_node;
	  let process_vc vc =
            add_child (xml_of_verification_atom vc) nonnegative_node
	  in
	    List.iter process_vc (termination_info.nonnegative_vcs); 
	    add_child nonnegative_node termination_node;
      end;
      termination_node
        (* Now we put together the root node *)
  and transmission_node = Xml_generator.create "piVC_transmission" in
  let overall_validity = (Verify.overall_validity_of_function_validity_information_list fns) in
    add_attribute ("type", "program_submission_response") transmission_node;    
    let messages_node = xml_of_messages messages in
      add_child messages_node transmission_node;
      let result_node = Xml_generator.create "result" in
        add_attribute ("status", Verify.string_of_validity overall_validity) result_node;
        add_child result_node transmission_node;
        let process_function func = 
          add_child (xml_of_function func) result_node
        in
          List.iter process_function fns;
          transmission_node
  
(* Wrapper for compile function that passes it the
   cache of VCs. *)
let get_main_server_func () =
  let vc_cache = Hashtbl.create (Config.get_value_int "cache_size") in
  let cache_lock = Mutex.create () in
  compile (vc_cache, cache_lock) ;;

let start_main_server () =
  Config.set_server_type Config.MainServer;
  Config.load (Utils.get_absolute_path Constants.main_server_config_file_path);
  run_server (get_main_server_func ()) (Config.get_value_int "port") ;;
