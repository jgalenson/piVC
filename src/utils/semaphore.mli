module Semaphore :
  sig
    type t = Condition.t * Mutex.t * int ref
    val init : int -> t
    val down : t -> unit
    val up : t -> unit
  end ;;
