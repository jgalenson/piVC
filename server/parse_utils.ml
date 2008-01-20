(* piVC *)

open Parser
open Ast
open Semantic_checking
open Lexing
open Utils

let parseToken lexbuf errors = 
   let program = Parser.main Lexer.lang lexbuf in
   check_program program errors ;;

(* Gets all the info we need from a program.
   That is, for each method, its basic paths and VCs: (path_node list * expr).list
   Returns (fnName as string * (Basic Path * VC) list) list. *)
let get_all_info program =
  (* Returns a pair of fnName and its basic path.
     Returns (string * path_node list list). *)
  let get_basic_paths program =
    let get_decl_paths_if_appropriate decl = 
      match decl with
          VarDecl (loc, vd) -> None
	| FnDecl (loc, fd) -> Some (fd.fnName.name, Basic_paths.generate_paths_for_func fd program)
    in
    (* Concatenate together functions ignoring vardecls. *)
    let map_fn all cur =
      let paths = get_decl_paths_if_appropriate cur in
	match paths with
	    None -> all
	  | Some (e) -> all @ [e]
    in
      List.fold_left map_fn [] program.decls
  in
  (* Add the VCs into the fnName + Basic path info. *)
  let get_vcs (fnName, paths) =
    (fnName, List.map (fun path -> (path, Verification_conditions.get_vc path)) paths)
  in
  let paths = get_basic_paths program in
  List.map get_vcs paths ;;

let parse lexbuf =
  let errors = Queue.create () in
  let e = ref errors in
  try
    let program = Parser.main Lexer.lang lexbuf in
    check_program program e;
    ((Some program), queue_to_list !e)
  with Parsing.Parse_error ->
    let symbol_start_pos = Lexing.lexeme_start_p lexbuf in
    let symbol_end_pos = Lexing.lexeme_end_p lexbuf in
    let loc = Ast.create_location symbol_start_pos symbol_end_pos in
    add_error SyntaxError (Lexing.lexeme lexbuf) loc e;
    (None, queue_to_list !e)
      
let goParse ic =
  let lexbuf = Lexing.from_channel ic in
  parse lexbuf ;;

let parse_string s =
  let lexbuf = Lexing.from_string s in
  parse lexbuf ;;
