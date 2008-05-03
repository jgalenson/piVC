(* main_server *)

open Verify ;;

val compile : ((string, ((Verify.validity * Counterexamples.example list option) * float)) Hashtbl.t * Mutex.t) -> in_channel -> out_channel -> unit ;;
val get_main_server_func : unit -> in_channel -> out_channel -> unit;;
