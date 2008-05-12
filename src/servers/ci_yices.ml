module A = Array

type id = int

let pr = print_endline
let soi = string_of_int

type manager =
    { ic : in_channel;
      oc : out_channel;
      fd : Unix.file_descr * Unix.file_descr;
      pid : int;
    }

let max_man = 32
let managers = A.make max_man None

let log = Unix.openfile ".yices.log" [Unix.O_CREAT; Unix.O_WRONLY] 0o640

let init () = ()

exception Done

let create () =
  let ip, op = Unix.pipe () in
  let im, om = Unix.pipe () in
    Unix.setsockopt im Unix.SO_REUSEADDR true;
    Unix.set_nonblock im;
    let pid = (Unix.create_process (Utils.get_absolute_path (Config.get_value "yices_path")) [|"yices"|] ip om log) in
      ignore(pid);(*doing this to supress unused variable warning*)
      Unix.close ip;
      Unix.close om;
      { ic = Unix.in_channel_of_descr im;
	oc = Unix.out_channel_of_descr op;
	fd = (im, op);
	pid = pid;
      }

let new_context () =
  let rv = ref (-1) in
    try
      for i = 0 to A.length managers - 1 do
	if managers.(i) = None then begin
	  managers.(i) <- Some (create ());
	  rv := i;
	  raise Done
	end
      done;
      raise Not_found
    with
      | Done -> !rv

let get i =
  assert (0 <= i && i < A.length managers);
  match managers.(i) with
    | None -> assert false
    | Some m -> m

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
  try
    let m = get i in
      input_line m.ic
  with
    | End_of_file
    | Sys_blocked_io -> assert(false)

let destroy i m =
  try 
    send i "(exit)";
    ignore (Unix.waitpid [] m.pid);
    let i, o = m.fd in
      Unix.close i;
      Unix.close o
  with
    | _ -> pr "destroy error"

let delete_context i =
  destroy i (get i);
  managers.(i) <- None

let reset_context i =
  destroy i (get i);
  managers.(i) <- Some (create ())
