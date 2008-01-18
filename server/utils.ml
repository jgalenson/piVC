(* Random utilities *)

let queue_to_list q =
  Queue.fold (fun a b -> a @ [b]) [] q ;;
