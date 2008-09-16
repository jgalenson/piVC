open Ast ;;

(* Gets the vcs that ensure that all the ranking annotations are always non-negative. *)
let get_nonnegativity_vcs fn program =
  (* First, get all the ranking annotations and their corresponding normal annotations. *)
  let all_ranking_annotations =
    (* We store ras as options, so we need to get them out.
       a is the corresponding normal annotation, since we need them together. *)
    let get_ra_from_opt opt annot =
      if (Utils.is_some opt) then
	[ (Utils.elem_from_opt opt, annot) ]
      else
	[]
    in
    (* Get all the ranking annotations (and their corresponding normal annotations) from a stmt.
       The result is the new ones concatenated with prev_list so we can use List.fold_left. *)
    let rec get_ras_from_stmt prev_list s = match s with
      | WhileStmt (l, t, block, annot, ra) -> prev_list @ (get_ra_from_opt ra annot) @ (get_ras_from_stmt [] block)
      | ForStmt (l, init, test, incr, block, annot, ra) -> prev_list @ (get_ra_from_opt ra annot) @ (get_ras_from_stmt [] block)
      | IfStmt (l, t, s1, s2) -> prev_list @ (get_ras_from_stmt [] s1) @ (get_ras_from_stmt [] s2)
      | StmtBlock (l, stmts) -> prev_list @ get_ras_from_stmts stmts
      | _ -> prev_list
    and get_ras_from_stmts stmts =
      List.fold_left get_ras_from_stmt [] stmts
    in
    let fnAnnotation = get_ra_from_opt fn.fnRankingAnnotation fn.preCondition in
    let fn_ras = fnAnnotation @ (get_ras_from_stmt [] fn.stmtBlock) in
    fn_ras
  in
  (* Then get the list of all the (annotation -> ra is nonnegative) implications. *)
  let nonnegativity_implications = 
    let dummy_loc = Ast.get_dummy_location () in
    let zero = Ast.Constant (dummy_loc, Ast.ConstInt (dummy_loc, 0)) in
    (* Gets a single (annotation -> ra is nonnegative) implication. *)
    let single_implication prev (ra, annot) =
      let cur_ge =
	(* Gets a single >=0 from a tuple and And it with prev so we can use List.fold_left
	   to build up the whole implication. *)
	let single_ge prev e =
	  let cur_ge = Ast.GE (dummy_loc, e, zero) in
	    if (Utils.is_none prev) then
	      Some (cur_ge)
	    else
	      Some (Ast.And (dummy_loc, Utils.elem_from_opt prev, cur_ge))
	in
        List.fold_left single_ge None ra.tuple
      in
      (* The acual implication: the annotation implies all the >=s. *)
      assert (Utils.is_some cur_ge);
      let cur_implication = Ast.Implies (ra.location_ra, annot.ann, Utils.elem_from_opt cur_ge) in
      let replaced_implication = Verification_conditions.replace_length_and_members_with_var cur_implication program in
      prev @ [ (ra, replaced_implication) ]
    in
    List.fold_left single_implication [] all_ranking_annotations
  in
  nonnegativity_implications ;;
