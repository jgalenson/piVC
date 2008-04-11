(* Computes a function in the background
   on its own thread and returns the result. *)

module Background =
  struct
    
    type 'a t = 'a Event.channel

    let create f args =
      let worker chan =
	let result = f args in
	Event.sync (Event.send chan (result))
      in
      let c = Event.new_channel () in
      ignore (Thread.create worker c);
      c

    let get_result t =
      Event.sync (Event.receive t)
    
  end;;
