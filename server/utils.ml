(* Random utilities *)

(* Converts a queue to a list. *)
let queue_to_list q =
  Queue.fold (fun a b -> a @ [b]) [] q ;;
