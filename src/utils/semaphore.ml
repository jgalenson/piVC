module Semaphore =
  struct

    type t = (Condition.t * Mutex.t * int ref)

    let init initial_val =
      assert (initial_val >= 0);
      let m = Mutex.create () in
      let c = Condition.create () in
      let counter = ref initial_val in
      (c, m, counter)

    let down (c, m, counter) =
      Mutex.lock m;
      while !counter <= 0 do
	Condition.wait c m;
      done;
      decr counter;
      Mutex.unlock m

    let up (c, m, counter) =
      Mutex.lock m;
      incr counter;
      Condition.signal c;
      Mutex.unlock m

  end ;;
