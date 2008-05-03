exception PiException of exn * string * int * int ;;

(* Asserts asrt and raises ex if it fails.
   Usage example: our_assert (lazy (assert (1=2))) (TestEx("hi", 42)); *)
let our_assert asrt ex =
 try
  ignore (Lazy.force asrt);
 with Assert_failure(file, line, col) ->
   raise (PiException (ex, file, line, col)) ;;

(* Gets the string of an exception.
   If it's a PiException, we print out the embedded
   exception as well as its file, line, and col.
   Otherwise, we use Printexc to print it. *)
let string_of_exception ex =
  match ex with
    | PiException (ex, file, line, col) ->
	let line_str = string_of_int line in
	let col_str = string_of_int col in
        "Exception " ^ (Printexc.to_string ex) ^ " in file " ^ file ^ ", line " ^ line_str ^ ", col " ^ col_str ^ "."
    | x -> Printexc.to_string x ;;
