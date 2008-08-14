exception PiExceptionEx of exn * string * int * int ;;
exception PiExceptionStr of string * string * int * int ;;

(* Asserts asrt and raises ex if it fails.
   Usage example: our_assert_ex (lazy (assert (1=2))) (TestEx("hi", 42)); *)
let our_assert_ex asrt ex =
 try
  ignore (Lazy.force asrt);
 with Assert_failure(file, line, col) ->
   raise (PiExceptionEx (ex, file, line, col)) ;;

(* Asserts asrt and raises ex if it fails.
   Usage example: our_assert_str (lazy (assert (1=2))) "You shouldn't assert false"; *)
let our_assert_str asrt msg =
 try
  ignore (Lazy.force asrt);
 with Assert_failure(file, line, col) ->
   raise (PiExceptionStr (msg, file, line, col)) ;;

(* Gets the string of an exception.
   If it's a PiException, we print out the embedded
   exception as well as its file, line, and col.
   Otherwise, we use Printexc to print it. *)
let rec string_of_exception ex =
  match ex with
    | PiExceptionEx (ex, file, line, col) ->
	let line_str = string_of_int line in
	let col_str = string_of_int col in
        "Exception " ^ (string_of_exception ex) ^ " in file " ^ file ^ ", line " ^ line_str ^ ", col " ^ col_str ^ "."
    | PiExceptionStr (msg, file, line, col) ->
	let line_str = string_of_int line in
	let col_str = string_of_int col in
        "Error \"" ^ msg ^ "\" in file " ^ file ^ ", line " ^ line_str ^ ", col " ^ col_str ^ "."
    | Unix.Unix_error (error, s1, s2) ->
	"Unix error: " ^ Unix.error_message error
    | x -> Printexc.to_string x ;;
