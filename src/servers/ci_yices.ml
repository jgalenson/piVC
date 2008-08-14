module A = Array

type id = int

let pr = print_endline
let soi = string_of_int

type manager =
    { ic : in_channel;
      oc : out_channel;
      fd : Unix.file_descr * Unix.file_descr;
      pid : int;
      error : Unix.file_descr
    }

let max_man = 5000
let managers = A.make max_man None
let managers_lock = Mutex.create ()

let init () = ()

exception Done
exception YicesNotFound of string
exception YicesError of string

let create () =
  let ip, op = Unix.pipe () in
  let im, om = Unix.pipe () in
  let error_out, error_in = Unix.pipe () in
    Unix.set_nonblock im;
    let yices_executable =
      let config_value = Config.get_value "yices_path" in
      if (String.contains config_value '/') then
	Utils.get_absolute_path config_value
      else
	config_value
    in
    let pid = (Unix.create_process yices_executable [|"yices"|] ip om error_in) in
      (* Ensure yices was successfully started. *)
      let (killed_pid, _) = Unix.waitpid [Unix.WNOHANG; Unix.WUNTRACED] pid in
      if killed_pid != 0 then begin
	raise (YicesNotFound yices_executable)
      end;
      Unix.close ip;
      Unix.close om;
      Unix.close error_in;
      { ic = Unix.in_channel_of_descr im;
	oc = Unix.out_channel_of_descr op;
	fd = (im, op);
	pid = pid;
	error = error_out;
      }

let new_context () =
  let rv = ref (-1) in
    try
      Mutex.lock managers_lock;
      for i = 0 to A.length managers - 1 do
	if managers.(i) = None then begin
	  managers.(i) <- Some (create ());
	  rv := i;
	  raise Done
	end
      done;
      raise Not_found
    with
      | Done ->
	  Mutex.unlock managers_lock;
	  !rv
      | Not_found ->
	  Mutex.unlock managers_lock;
	  raise Not_found
      | ex ->
	  Mutex.unlock managers_lock;
	  raise ex

let get i =
  Mutex.lock managers_lock;
  assert (0 <= i && i < A.length managers);
  match managers.(i) with
    | None ->
	Mutex.unlock managers_lock;
	assert false
    | Some m ->
	Mutex.unlock managers_lock;
	m

let send i msg =
  (*pr ((soi i) ^ ": " ^ msg);*)
  let m = get i in
    output_string m.oc (msg ^ "\n");
    flush m.oc

let wait i =
  (*pr ("Waiting on " ^ (soi i) ^ "...");*)
  let m = get i in
  let fd, _ = m.fd in
    ignore (Unix.select [fd] [] [] (-1.))

let recv i =
  let m = get i in
  try
    input_line m.ic
  with
    | End_of_file
    | Sys_blocked_io ->
	let str = input_line (Unix.in_channel_of_descr m.error) in
	raise (YicesError (str))

let destroy i m =
  try 
    send i "(exit)";
    ignore (Unix.waitpid [] m.pid);
    let i, o = m.fd in
      Unix.close i;
      Unix.close o;
      Unix.close m.error
  with
    | _ -> pr "destroy error"

let delete_context i =
  destroy i (get i);
  Mutex.lock managers_lock;
  managers.(i) <- None;
  Mutex.unlock managers_lock

let reset_context i =
  destroy i (get i);
  Mutex.lock managers_lock;
  managers.(i) <- Some (create ());
  Mutex.unlock managers_lock
