(* Random utilities *)

exception Option_Is_None

let elem_from_opt opt = match opt with
    Some(something) -> something
  | None -> raise Option_Is_None

(* Converts a queue to a list. *)
let queue_to_list q =
  Queue.fold (fun a b -> a @ [b]) [] q 
