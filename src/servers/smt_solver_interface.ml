type solver_interface = {
  ic : in_channel;
  oc : out_channel;
  fd : Unix.file_descr * Unix.file_descr;
  pid : int;
  error : Unix.file_descr
}

exception SolverNotFound of string

let get_filename base_filename =
  base_filename ^ "_" ^ string_of_int (Unix.getpid ()) ;;

let create () =
  let ip, op = Unix.pipe () in
  let im, om = Unix.pipe () in
  let error_out, error_in = Unix.pipe () in
  Unix.set_nonblock im;
  let solver_executable =
    let config_value = Config.get_value "smt_solver_path" in
    if (String.contains config_value '/') then
      Utils.get_absolute_path config_value
    else
      config_value
  in
  (* Ensure that the solver file exists and is executable. *)
  let can_run_solver () =
    if not (Sys.file_exists solver_executable) then
      false
    else begin
      try
	Unix.access solver_executable [Unix.X_OK];
	true
      with
	| Unix.Unix_error(_, _, _) -> false
    end
  in
  if not (can_run_solver ()) then begin
    raise (SolverNotFound solver_executable)
  end;
  (* If we're communicating with the SMT solver with a file, we have to
     append the filename to the argument list.  Since the filename includes
     our pid (since we can have many SMT servers at once it must be unique),
     we have to do this here and not earlier. *)
  let arguments =
    match Smt_solver.get_input_method () with
      | Smt_solver.Stdin ->
	  Smt_solver.get_arguments()
      | Smt_solver.File (base_filename) ->
	  let filename = get_filename base_filename in
	  if not (Sys.file_exists filename) then
	    failwith ("File " ^ filename ^ " used to communicate with the SMT solver does not exist.");
	  Array.append (Smt_solver.get_arguments ()) [| filename |]
  in
  (*print_endline (Array.fold_left (fun acc cur -> if acc = "" then cur else acc ^ " " ^ cur) "" arguments);*)
  let pid = (Unix.create_process solver_executable arguments ip om error_in) in
  Unix.close ip;
  Unix.close om;
  Unix.close error_in;
  {
    ic = Unix.in_channel_of_descr im;
    oc = Unix.out_channel_of_descr op;
    fd = (im, op);
    pid = pid;
    error = error_out;
  }

let write_to_file msg base_filename = 
  let filename = get_filename base_filename in
  (* If the file already exists, fail. *)
  if Sys.file_exists filename then
    failwith ("File " ^ filename ^ " used to communicate with the SMT solver already exists.");
  let fd = Unix.openfile filename [Unix.O_WRONLY; Unix.O_CREAT] 0o644 in
  ignore (Unix.write fd msg 0 (String.length msg));
  Unix.close fd

let send m msg =
  output_string m.oc (msg ^ "\n");
  flush m.oc

let wait m =
  let fd, _ = m.fd in
  let timeout = float_of_int (Config.get_value_int "timeout_time") in
  let can_read, _, _ = Unix.select [fd] [] [] timeout in
  if (List.length can_read) = 0 then
    raise Smt_solver.SolverTimeout

let recv m =
  try
    input_line m.ic
  with
    | End_of_file
    | Sys_blocked_io ->
	let get_line_of_error () =
	  input_line (Unix.in_channel_of_descr m.error)
	in
	let error_msg = Smt_solver.parse_error get_line_of_error in
	raise (Smt_solver.SolverError (error_msg))

let finish m =
  let shutdown_command = Smt_solver.get_shutdown_command () in
  if Utils.is_some shutdown_command then
    send m (Utils.elem_from_opt shutdown_command);
  ignore (Unix.waitpid [] m.pid);
  begin
    match Smt_solver.get_input_method () with
      | Smt_solver.File (base_filename) ->
	  let filename = get_filename base_filename in
	  Sys.remove filename
      | _ -> ()
  end;
  let i, o = m.fd in
  Unix.close i;
  Unix.close o;
  Unix.close m.error

let kill m =
  Unix.kill m.pid Sys.sigkill

let get_response_from_smt_solver input =
  (* If we're communicating with files, we have to be sure we delete
     the file even if the server is terminated in the middle of execution. *)
  begin
    match Smt_solver.get_input_method () with
      | Smt_solver.File (base_filename) ->
	  let filename = get_filename base_filename in
	  let cleanup_on_break_fn () =
	    if Sys.file_exists filename then
	      Sys.remove filename
	  in
	  at_exit cleanup_on_break_fn
      | _ -> ()
  end;
  (* Start and return the solver.  We have to special case stdin vs. file
     communication since the latter require us to write the file before
     we start the SMT solver itself. *)
  let solver =
    match Smt_solver.get_input_method () with
      | Smt_solver.Stdin ->
	  let s = create () in
	  send s input;
	  s
      | Smt_solver.File (base_filename) ->
	  write_to_file input base_filename;
	  create ()
  in
  (* Wait for and get the response. *)
  begin
    try
      wait solver
    with
      | ex ->
	  kill solver;
	  raise ex
  end;
  let get_line_of_output should_wait =
    if should_wait then
      wait solver;
    recv solver
  in
  let (response, counterexample_opt) = Smt_solver.parse_output get_line_of_output in
  finish solver;
  (response, counterexample_opt) ;;
