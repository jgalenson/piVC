open Xml_generator
open Utils
open Ast
open Semantic_checking
open Server_framework
open Net_utils

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

  print_endline "Beginning compile.";
  let code = get_input ic in
  (* print_endline code; *)
  let (program, errors) = Parse_utils.parse_string code in


  let get_output_to_return_to_client = 
    match errors with
        [] -> (
          let program_info = Verify.get_all_info (Utils.elem_from_opt program) in
          let verified_program_info = Verify.verify_program program_info vc_cache_and_lock in
            Xml_generator.string_of_xml_node (xml_of_verified_program verified_program_info)
              )
      | _  -> Xml_generator.string_of_xml_node (xml_of_errors errors)

  in
    send_output oc get_output_to_return_to_client;
  flush oc;
  print_endline "End compile.";


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
  let rec xml_of_function (fn, all_valid, basic_paths) = 
    let function_node = Xml_generator.create "function" in
      add_attribute ("name", fn.fnName.name) function_node;
      add_attribute ("status", Verify.string_of_validity all_valid) function_node;
      add_child (xml_of_location fn.location_fd) function_node;
      let process_basic_path basic_path = 
        add_child (xml_of_basic_path basic_path) function_node in
        List.iter process_basic_path basic_paths;
        function_node
  and xml_of_basic_path (nodes, vc, valid, counterexample) =
    let basic_path_node = Xml_generator.create "basic_path" in
      add_attribute ("status", Verify.string_of_validity valid) basic_path_node;
      let path_node = Xml_generator.create "path" in
        add_child path_node basic_path_node;
        let vc_node = Xml_generator.create "vc" in
          set_text (Ast.string_of_expr vc) vc_node;
          add_child vc_node basic_path_node;
	  if (Utils.is_some counterexample) then
	    begin
	      let counterexample_node = Xml_generator.create "counterexample" in
	      set_text (Counterexamples.counterexample_to_string (Utils.elem_from_opt counterexample)) counterexample_node;
	      add_child counterexample_node basic_path_node;
	    end;
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
  (* Now we put together the root node *)
  and transmission_node = Xml_generator.create "piVC_transmission" in
    add_attribute ("type", "program_submission_response") transmission_node;
    let result_node = Xml_generator.create "result" in
      add_attribute ("status", Verify.string_of_validity all_valid) result_node;
      add_child result_node transmission_node;
      let process_function func = 
        add_child (xml_of_function func) result_node in
        List.iter process_function functions;
        transmission_node ;;

(* Wrapper for compile function that passes it the
   cache of VCs. *)
let get_main_server_func () =
  let vc_cache = Hashtbl.create Constants.num_cached_vcs in
  let cache_lock = Mutex.create () in
  compile (vc_cache, cache_lock)
