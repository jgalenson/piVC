exception PiException of exn * string * int * int ;;

val our_assert : 'a Lazy.t -> exn -> unit ;;
val string_of_exception : exn -> string ;;
